package com.outr.jsd2sjs

import java.io.File

import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import org.powerscala.io.IO

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
    val articles = doc.select("body > div[id=main] > section")
    val matches = articles.select("article > h4[class=name], article > div[class=description]")
    val items = (0 until matches.size()).map(matches.get(_).text().trim).toList.grouped(2).flatMap(objectInfo).toList
    val packageName = "com.outr.fabric"
    val completePackage = path.replace('/', '.').substring(6) match {
      case s => s.substring(0, s.length - 5)
    }
    val jsPackage = completePackage.substring(0, completePackage.lastIndexOf('.'))
    val name = completePackage.substring(completePackage.indexOf('.') + 1)
    generate(packageName, jsPackage, name, items)
  }

  def objectInfo(items: List[String]): Option[ObjectInfo] = {
    val heading = items.head
    val colon = heading.indexOf(':')
    if (colon == -1) {              // Method
      val name = heading.substring(0, heading.indexOf('('))
      val args = heading.substring(heading.indexOf('(') + 1, heading.indexOf(')')).split(",").map(_.trim).toList
      val index = heading.indexOf('{')
      val returnType = if (index > -1) {
        val s = heading.substring(index + 1, heading.indexOf('}', index))
        val lastDot = s.lastIndexOf('.')
        if (lastDot > -1) {
          s.substring(lastDot + 1)
        } else {
          s
        }
      } else {
        "Unit"
      }
      val description = items.tail.head.trim
      Some(MethodInfo(name, args, returnType, description))
    } else {                        // Var
      val name = heading.substring(0, colon).trim
      val className = heading.substring(colon + 1).trim
      val description = items.tail.head.trim
      Some(VarInfo(name, className, description))
    }
  }

  def generate(packageName: String, jsPackage: String, name: String, entries: List[ObjectInfo]): String = {
    val b = new StringBuilder
    b.append(s"package $packageName\n\n")
    b.append("import scala.scalajs.js\n")
    b.append("import scala.scalajs.js.annotation.JSName\n\n")
    b.append("@js.native\n")
    b.append(s"""@JSName("$jsPackage.$name")\n""")
    b.append(s"class $name extends js.Object {\n")
    entries.foreach {
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

trait ObjectInfo

case class VarInfo(name: String, className: String, description: String) extends ObjectInfo

case class MethodInfo(name: String, args: List[String], returnType: String, description: String) extends ObjectInfo