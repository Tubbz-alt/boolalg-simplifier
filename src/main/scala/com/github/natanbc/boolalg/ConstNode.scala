package com.github.natanbc.boolalg

import spray.json.{ JsNumber, JsObject, JsString }

class ConstNode(var id: Int, val value: Boolean) extends Node {
  override def simplify(context: SimplificationContext, nested: Boolean): Node = this
  
  override def eval: Option[Boolean] = Some(value)
  
  override def toJson: JsObject = JsObject(
    "type" -> JsString("CONST"),
    "id" -> JsNumber(id),
    "value" -> JsNumber(hashCode())
  )
  
  override def toString: String = {
    if(value) {
      "1"
    } else {
      "0"
    }
  }
  
  override def hashCode(): Int = {
    if(value) {
      1
    } else {
      0
    }
  }
  
  override def equals(obj: Any): Boolean = canEqual(obj) && obj.asInstanceOf[ConstNode].value == value
  
  override def canEqual(that: Any): Boolean = that.isInstanceOf[ConstNode]
}

object ConstNode {
  def unapply(arg: ConstNode): Option[Boolean] = Some(arg.value)
}
