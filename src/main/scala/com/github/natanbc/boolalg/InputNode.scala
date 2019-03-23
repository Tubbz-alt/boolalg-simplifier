package com.github.natanbc.boolalg

class InputNode(val id: Int, val name: String) extends Node {
    override def simplify(context: SimplificationContext, nested: Boolean): Node = this

    override def eval: Option[Boolean] = None

    override def toString: String = name

    override def canEqual(that: Any): Boolean = that.isInstanceOf[InputNode]

    override def hashCode(): Int = name.hashCode()

    override def equals(obj: Any): Boolean = canEqual(obj) && obj.asInstanceOf[InputNode].name == name
}

object InputNode {
    def unapply(arg: InputNode): Option[String] = Some(arg.name)
}
