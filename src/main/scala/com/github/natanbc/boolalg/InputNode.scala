package com.github.natanbc.boolalg

import spray.json.{ JsNumber, JsObject, JsString }

class InputNode(var id: Int, val name: String) extends Node {
  override def simplify(context: SimplificationContext, nested: Boolean): Node = this
  
  override def eval: Option[Boolean] = None
  
  override def toJson: JsObject = JsObject(
    "type" -> JsString("INPUT"),
    "id" -> JsNumber(id),
    "name" -> JsString(name)
  )
  
  override def toString: String = name
  
  override def hashCode(): Int = name.hashCode()
  
  override def equals(obj: Any): Boolean = canEqual(obj) && obj.asInstanceOf[InputNode].name == name
  
  override def canEqual(that: Any): Boolean = that.isInstanceOf[InputNode]
}

object InputNode {
  def unapply(arg: InputNode): Option[String] = Some(arg.name)
}
