package util

import org.pegdown._
import org.pegdown.ast._
import scala.collection.JavaConverters._

class PegDownProc(options: Int) extends PegDownProcessor(options) {
  override def markdownToHtml(markdownSource: Array[Char], linkRenderer: LinkRenderer): String = {
    val asRoot = parseMarkdown(markdownSource)
    val serializer = new NumberingSerializer(linkRenderer)
    val content = serializer.toHtml(asRoot)
    content + """
<div id="toc" class="btn-group">
  <button class="btn btn-mini btn-info dropdown-toggle" data-toggle="dropdown">
    TOC <span class="caret"></span>
  </button>
  <ul class="dropdown-menu nav">""" + serializer.toc + """
  </ul>
</div>"""
  }
}

class NumberingSerializer(linkRenderer: LinkRenderer) extends ToHtmlSerializer(linkRenderer) {

  val toc = new StringBuilder
  
  val HMAX = 6
  /**
   * Current numbering.
   */
  protected val numbering = new Array[Int](HMAX)
  
  def nextNumber(level: Int, text: String) = {
    numbering(level - 1) += 1
    for (i <- level until HMAX) numbering(i) = 0
    val id = numbering.take(level).mkString("")
    val num = numbering.take(level).mkString(".")
    toc ++= """<li><a href="#%s">%s %s</a></li>""".format(id, num, text)
    (id, num)
  }

  override def visit(node: HeaderNode) {
    val text = node.getChildren.asScala.headOption match {
      case Some(n) =>
        n match {
          case tn: TextNode => tn.getText
          case _ => ""
        }
      case None => ""
    }
    val (id, num) = nextNumber(node.getLevel, text)
    printer.print("""<a name="%s" id="%s"></a>""".format(num, id))
    node.getChildren.asScala.headOption match {
      case Some(n) =>
        n match {
          case tn: TextNode =>
          	printTag(new TextNode(num + " " + tn.getText), 
          	    "h" + node.getLevel());
          case _ =>
          	printTag(node, "h" + node.getLevel());
        }
      case None =>
        printTag(node, "h" + node.getLevel());
    }
  }
}