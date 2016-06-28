package com.outr.jsd2sjs

import java.io.File
import java.net.URL

import com.outr.scribe.Logging
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import org.jsoup.select.Elements
import org.powerscala.io._

import scala.annotation.tailrec

object JSDocParser extends Logging {
  val cache = new File("cache")
  val baseUrl = "http://fabricjs.com"
  val outDir = new File("/home/mhicks/projects/open-source/scalajs-fabricjs/src/main/scala/com/outr/fabric/")

  def main(args: Array[String]): Unit = {
    cache.mkdirs()
    val file = new File(cache, "index.html")
    if (!file.exists()) {
      val url = s"$baseUrl/docs/index.html"
      IO.stream(new URL(url), file)
    }
    val doc = Jsoup.parse(file, "UTF-8")
    val matches = doc.select("body section article ul li a")
    val anchors = (0 until matches.size()).map(matches.get).toList
    anchors.foreach(process)
  }

  val ignore = Set(
    "fabric", "fabric.util", "fabric.util.object", "fabric.util.array", "fabric.util.string", "fabric.util.ease",
    "fabric.Image", "fabric.Image.filters", "fabric.Image.filters.BaseFilter", "fabric.Image.filters.Brightness",
    "fabric.Image.filters.Convolute", "fabric.Image.filters.GradientTransparency", "fabric.Image.filters.Grayscale",
    "fabric.Image.filters.Invert", "fabric.Image.filters.Mask", "fabric.Image.filters.Noise",
    "fabric.Image.filters.Pixelate", "fabric.Image.filters.RemoveWhite", "fabric.Image.filters.Sepia",
    "fabric.Image.filters.Sepia2", "fabric.Image.filters.Tint"
  )

  def process(element: Element): Unit = {
    val path = element.attr("href")
    val completePackage = path.replace('/', '.').substring(6) match {
      case s => s.substring(0, s.length - 5)
    }
    if (!ignore.contains(completePackage)) {
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
      val matches = articles.select("article > h4[class=name]").toList
      val items = matches.flatMap(element2ObjectInfo)

      val packageName = "com.outr.fabric"
      val extending = doc.select("h3[class=subsection-title] + ul > li > a").text() match {
        case "" => "js.Object"
        case s => fixType(s)
      }
      val data = generate(packageName, params, extending, jsPackage, name, description, items)
      val filename = s"$name.scala"
      IO.stream(data, new File(outDir, filename))
    }
  }
  val VarTypeRegex = """[:](.+)""".r

  val classNameMap = Map(
    "delegatedProperties" -> "String",
    "enableRetinaScaling" -> "Boolean",
    "getSvgSrc" -> "String",
    "type" -> "Object",
    "colorNameMap" -> "Object",
    "reHex" -> "js.Function",
    "reHSLa" -> "js.Function",
    "reRGBa" -> "js.Function",
    "reOffsetsAndBlur" -> "js.Function",
    "ATTRIBUTE_NAMES" -> "js.Array[String]",
    "DEFAULT_SVG_FONT_SIZE" -> "Double",
    "getElementStyle" -> "String",
    "SVGElement" -> "org.scalajs.dom.raw.SVGElement"
  )

  val ignoreNames = Set("toString", "type", "rotate -&gt; setAngle")

  def element2ObjectInfo(element: Element): Option[ObjectInfo] = {
    val name = element.childNode(1).toString
    val c1 = element.child(1)
    val description = element.nextElementSibling().text()
    val details = detailsFor(element)
    val isOverride = details.select("dl > dt[class=tag-overrides]").text().trim == "Overrides:"
    val isStatic = element.child(0).text().trim == "(static)"
    if (ignoreNames.contains(name) || isOverride) {
      None
    } else {
      logger.debug(s"Processing $element...")
      c1.attr("class") match {
        case "type-signature" => {
          val className = classNameMap.getOrElse(name, c1.text().trim match {
            case VarTypeRegex(s) => s
            case "" => throw new RuntimeException(s"Unspecified variable type ($name) - $element")
          })
          Some(VarInfo(isStatic, fixName(name), fixType(className), description))
        }
        case "signature" => {
          val descriptionElement = element.nextElementSibling()
          val table = descriptionElement.nextElementSibling().nextElementSibling()
          val args = if (table == null) {
            name match {
              case "onMouseUp" => List(s"pointer: ${fixType("Object")}")
              case "render" => List.empty
              case _ => throw new RuntimeException(s"Args table not found for: $name")
            }
          } else {
            val argNames = table.select("> tbody > tr > td[class=name]").toTextList
            val argTypes = table.select("> tbody > tr > td[class=type]").toTextList
            argNames.zip(argTypes).map {
              case (n, t) => s"${fixName(n)}: ${fixType(t)}"
            }
          }
          val returnType = fixType(element.select("a").text().trim)
          Some(MethodInfo(isStatic, name, args, returnType, description))
        }
      }
    }
  }

  @tailrec
  final def detailsFor(element: Element): Element = {
    if (element.attr("class").trim == "details") {
      element
    } else {
      detailsFor(element.nextElementSibling())
    }
  }

  def fixName(name: String): String = name match {
    case "object" => "`object`"
    case "type" => "`type`"
    case _ => name
  }

  def fixType(classType: String): String = classType match {
    case s if s.contains("|") && s.contains("String") => "String"
    case s if s.contains("|") && s.contains("Number") => "Double"
    case s if s.contains("|") && s.contains("function") => "js.Function"
    case s if s.startsWith("fabric.") => s.substring(7)
    case "Array" => "js.Array"
    case "Event" => "org.scalajs.dom.Event"
    case "Object" | "object" => "js.Object"
    case "CanvasRenderingContext2D" => "org.scalajs.dom.CanvasRenderingContext2D"
    case "HTMLCanvasElement" => "org.scalajs.dom.raw.HTMLCanvasElement"
    case "function" => "js.Function"
    case "Self" => "Unit"
    case "" => "js.Object"
    case s => s
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
    val local = entries.filterNot(e => e.name == "initialize" || e.static)
    val static = entries.filterNot(e => e.name == "initialize" || !e.static)
    local.foreach {
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
    if (static.nonEmpty) {
      b.append(s"\n\nobject $name {\n")
      static.foreach {
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
    }
    b.toString()
  }

  implicit class SuperElements(elements: Elements) {
    def toList: List[Element] = (0 until elements.size()).map(elements.get).toList
    def toTextList: List[String] = toList.map(_.text().trim)
  }
}

trait ObjectInfo {
  def name: String
  def static: Boolean
}

case class VarInfo(static: Boolean, name: String, className: String, description: String) extends ObjectInfo

case class MethodInfo(static: Boolean, name: String, args: List[String], returnType: String, description: String) extends ObjectInfo