package com.outr.jsd2sjs

import java.io.File
import java.net.URL

import com.outr.scribe.Logging
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import org.jsoup.select.Elements
import org.powerscala.io._

import scala.collection.mutable.ListBuffer

object JSDocParser extends Logging {
  val cache = new File("cache")
  val baseUrl = "http://fabricjs.com"
  val outDir = new File("/home/mhicks/projects/open-source/scalajs-fabricjs/src/main/scala/com/outr/fabric/")

  def main(args: Array[String]): Unit = {
    cache.mkdirs()
    val url = s"$baseUrl/docs/index.html"
    val doc = Jsoup.connect(url).get()
    val matches = doc.select("body section article ul li a")
    val anchors = (0 until matches.size()).map(matches.get).toList
    anchors.foreach(process)
  }

  def process(element: Element): Unit = {
    val path = element.attr("href")
    val completePackage = path.replace('/', '.').substring(6) match {
      case s => s.substring(0, s.length - 5)
    }
    val jsPackage = completePackage.substring(0, completePackage.lastIndexOf('.'))
    val name = completePackage.substring(completePackage.indexOf('.') + 1)
    logger.info(s"Processing $name...")
    val file = new File(cache, s"$name.html")
    if (!file.exists()) {
      val url = s"$baseUrl$path"
      IO.stream(new URL(url), file)
    }
    val doc = Jsoup.parse(file, "UTF-8")
    val description = doc.select("body > div[id=main] > section > article > div[class=container-overview] > div[class=description]").text().trim
    val params = doc.select("body > div[id=main] > section > article > div[class=container-overview] > table[class=params] > tbody > tr > td").toTextList.grouped(3).map(l => s"${fixName(l.head)}: ${fixType(l.tail.head)}").toList
    val articles = doc.select("body > div[id=main] > section")
    val matches = articles.select("article > h4[class=name], article > div[class=description], article > table[class=params] > tbody > tr > td[class=type]")
    val iterator = (0 until matches.size()).map(matches.get(_).text().trim).toIterator
    var items = ListBuffer.empty[ObjectInfo]
    while (iterator.hasNext) {
      objectInfo(iterator).foreach(oi => items += oi)
    }
    val packageName = "com.outr.fabric"
    val extending = doc.select("h3[class=subsection-title] + ul > li > a").text() match {
      case "" => "js.Object"
      case s => fixType(s)
    }
    val data = generate(packageName, params, extending, jsPackage, name, description, items.toList)
    val filename = s"$name.scala"
    IO.stream(data, new File(outDir, filename))
  }

  def fixName(name: String): String = name match {
    case "object" => "`object`"
    case _ => name
  }

  def fixType(classType: String): String = classType match {
    case s if s.contains("|") && s.contains("String") => "String"
    case s if s.startsWith("fabric.") => s.substring(7)
    case "Event" => "org.scalajs.dom.Event"
    case "Object" => "js.Object"
    case "CanvasRenderingContext2D" => "org.scalajs.dom.CanvasRenderingContext2D"
    case "HTMLCanvasElement" => "org.scalajs.dom.raw.HTMLCanvasElement"
    case "function" => "js.Function"
    case "Self" => "Unit"
    case s => s
  }

  def objectInfo(iterator: Iterator[String]): Option[ObjectInfo] = {
    val heading = iterator.next()

//    if (heading.startsWith("(static)")) {
//      if (heading.indexOf("(", 2) != -1) {
//        iterator.next()   // Skip two lines if it's a static method
//        iterator.next()
//      }
//       Ignore static
//      None
//    } else {
      logger.info(s"Starting with: $heading")
      try {
        val colon = heading.indexOf(':')
        if (heading.contains("(")) {
          // Method
          val name = heading.substring(0, heading.indexOf('('))
          val args = heading.substring(heading.indexOf('(') + 1, heading.indexOf(')')).split(",").map(_.trim).toList.collect {
            case s if s.nonEmpty => fixName(s)
          }
          val index = heading.indexOf('{')
          val returnType = if (index > -1) {
            heading.substring(index + 1, heading.indexOf('}', index))
          } else {
            "Unit"
          }
          val description = iterator.next()
          val argsWithTypes = args.map {
            case argName if argName.startsWith("...") || argName.startsWith("…") => {
              s"${argName.substring(3)}: ${fixType(iterator.next())}*"
            }
            case argName => s"$argName: ${fixType(iterator.next())}"
          }
          if (name == "toString") {
            None
          } else {
            Some(MethodInfo(fixName(name), argsWithTypes, fixType(returnType), description))
          }
        } else {
          // Var
          val (name, className) = heading match {
            case "enableRetinaScaling" => heading -> "Boolean"
            case s => s.substring(0, colon).trim -> fixType(s.substring(colon + 1).trim)
          }
          val description = iterator.next()
          Some(VarInfo(fixName(name), className, description))
        }
      } catch {
        case t: Throwable => throw new RuntimeException(s"Failed to parse on $heading.", t)
      }
//    }
  }

  def generate(packageName: String, params: List[String], extending: String, jsPackage: String, name: String, description: String, entries: List[ObjectInfo]): String = {
    val b = new StringBuilder
    b.append(s"package $packageName\n\n")
    b.append("import scala.scalajs.js\n")
    b.append("import scala.scalajs.js.annotation.JSName\n\n")
    b.append(s"/**\n")
    b.append(s"  * $description\n")
    b.append(s"  */\n")
    b.append("@js.native\n")
    b.append(s"""@JSName("$jsPackage.$name")\n""")
    val args = entries.find(_.name == "initialize") match {
      case Some(oi) => oi.asInstanceOf[MethodInfo].args
      case None => params
    }
    b.append(s"class $name(${args.mkString(", ")}) extends $extending {\n")
    entries.filterNot(_.name == "initialize").foreach {
      case info: VarInfo => {
        b.append(s"  /**\n")
        b.append(s"    * ${info.description}\n")
        b.append(s"    */\n")
        b.append(s"  var ${info.name}: ${info.className} = js.native\n")
      }
      case info: MethodInfo => {
        b.append(s"  /**\n")
        b.append(s"    * ${info.description}\n")
        b.append(s"    */\n")
        b.append(s"  def ${info.name}(${info.args.mkString(", ")}): ${info.returnType} = js.native\n")
      }
    }
    b.append("}")
    b.toString()
  }

  implicit class SuperElements(elements: Elements) {
    def toList: List[Element] = (0 until elements.size()).map(elements.get).toList
    def toTextList: List[String] = toList.map(_.text().trim)
  }
}

trait ObjectInfo {
  def name: String
}

case class VarInfo(name: String, className: String, description: String) extends ObjectInfo

case class MethodInfo(name: String, args: List[String], returnType: String, description: String) extends ObjectInfo