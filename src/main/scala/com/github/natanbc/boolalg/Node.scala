package com.github.natanbc.boolalg

import spray.json.JsObject

trait Node extends Equals {
  var id: Int
  
  def simplify(context: SimplificationContext, nested: Boolean = false): Node
  
  def eval: Option[Boolean]
  
  def toJson: JsObject
}

object Node {
  
  import scala.language.implicitConversions
  
  implicit val _nodeOrdering: Ordering[Node] = (x, y) => {
    (x, y) match {
      case (AndNode(a), AndNode(b)) => compare(a, b)
      case (AndNode(_), _) => -1
      case (OrNode(_), AndNode(_)) => 1
      case (OrNode(a), OrNode(b)) => compare(a, b)
      case (OrNode(_), _) => -1
      case (NegNode(_), AndNode(_)) | (NegNode(_), OrNode(_)) => 1
      case (NegNode(a), NegNode(b)) => _nodeOrdering.compare(a, b)
      case (NegNode(_), _) => -1
      case (InputNode(_), AndNode(_)) | (InputNode(_), OrNode(_)) | (InputNode(_), NegNode(_)) => 1
      case (InputNode(a), InputNode(b)) => b.compareTo(a)
      case (InputNode(_), _) => -1
      case (ConstNode(_), AndNode(_)) | (ConstNode(_), OrNode(_)) | (ConstNode(_), NegNode(_)) | (ConstNode(_),
      InputNode(_)) => 1
      case (ConstNode(a), ConstNode(b)) => (if(b) {
        1
      } else {
        0
      }) - (if(a) {
        1
      } else {
        0
      })
    }
  }
  
  private def compare(a: Seq[Node], b: Seq[Node]): Int = {
    for(i <- 0 until math.min(a.size, b.size)) {
      val c = _nodeOrdering.compare(a(i), b(i))
      if(c != 0) {
        return c
      }
    }
    a.size - b.size
  }
}
