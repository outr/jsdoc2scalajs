package com.outr.jsd2sjs

import java.io.File

import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import org.powerscala.io.IO

import scala.collection.mutable.ListBuffer

object JSDocParser {
  val baseUrl = "http://fabricjs.com"

  def main(args: Array[String]): Unit = {
    val url = s"$baseUrl/docs/index.html"
    val doc = Jsoup.connect(url).get()
    val matches = doc.select("body section article ul li a")
    val anchors = (0 until matches.size()).map(matches.get).toList
    val data = process(anchors.head)
    IO.stream(data, new File("/home/mhicks/projects/open-source/scalajs-fabricjs/src/main/scala/com/outr/fabric/Canvas.scala"))
  }

  def process(element: Element): String = {
    val path = element.attr("href")
    val url = s"$baseUrl$path"
    val doc = Jsoup.connect(url).get()
    val description = doc.select("body > div[id=main] > section > article > div[class=container-overview] > div[class=description]").text().trim
    val articles = doc.select("body > div[id=main] > section")
    val matches = articles.select("article > h4[class=name], article > div[class=description], article > table[class=params] > tbody > tr > td[class=type]")
    val iterator = (0 until matches.size()).map(matches.get(_).text().trim).toIterator
    var items = ListBuffer.empty[ObjectInfo]
    while (iterator.hasNext) {
      items += objectInfo(iterator)
    }
    val packageName = "com.outr.fabric"
    val completePackage = path.replace('/', '.').substring(6) match {
      case s => s.substring(0, s.length - 5)
    }
    val jsPackage = completePackage.substring(0, completePackage.lastIndexOf('.'))
    val name = completePackage.substring(completePackage.indexOf('.') + 1)
    generate(packageName, jsPackage, name, description, items.toList)
  }

  def fixName(name: String): String = name match {
    case "object" => "`object`"
    case _ => name
  }

  def fixType(classType: String): String = classType match {
    case s if s.startsWith("fabric.") => s.substring(7)
    case "Event" => "org.scalajs.dom.Event"
    case "Object" => "js.Object"
    case "CanvasRenderingContext2D" => "org.scalajs.dom.CanvasRenderingContext2D"
    case "HTMLCanvasElement" => "org.scalajs.dom.raw.HTMLCanvasElement"
    case "HTMLElement | String" => "String"
    case "Number | String" => "String"
    case s => s
  }

  def objectInfo(iterator: Iterator[String]): ObjectInfo = {
    val heading = iterator.next()

    try {
      val colon = heading.indexOf(':')
      if (colon == -1) {
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
        val argsWithTypes = args.map(argName => s"$argName: ${fixType(iterator.next())}")
        MethodInfo(fixName(name), argsWithTypes, fixType(returnType), description)
      } else {
        // Var
        val name = heading.substring(0, colon).trim
        val className = fixType(heading.substring(colon + 1).trim)
        val description = iterator.next()
        VarInfo(fixName(name), className, description)
      }
    } catch {
      case t: Throwable => throw new RuntimeException(s"Failed to parse on $heading.", t)
    }
  }

  def generate(packageName: String, jsPackage: String, name: String, description: String, entries: List[ObjectInfo]): String = {
    val b = new StringBuilder
    b.append(s"package $packageName\n\n")
    b.append("import scala.scalajs.js\n")
    b.append("import scala.scalajs.js.annotation.JSName\n\n")
    b.append(s"/**\n")
    b.append(s"  * $description\n")
    b.append(s"  */\n")
    b.append("@js.native\n")
    b.append(s"""@JSName("$jsPackage.$name")\n""")
    val initializer = entries.find(_.name == "initialize").get.asInstanceOf[MethodInfo]
    b.append(s"class $name(${initializer.args.mkString(", ")}) extends js.Object {\n")
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
}

trait ObjectInfo {
  def name: String
}

case class VarInfo(name: String, className: String, description: String) extends ObjectInfo

case class MethodInfo(name: String, args: List[String], returnType: String, description: String) extends ObjectInfo