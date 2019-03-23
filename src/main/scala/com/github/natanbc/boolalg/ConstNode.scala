package com.github.natanbc.boolalg

class ConstNode(val id: Int, val value: Boolean) extends Node {
    override def simplify(context: SimplificationContext, nested: Boolean): Node = this

    override def eval: Option[Boolean] = Some(value)

    override def toString: String = if(value) "1" else "0"

    override def canEqual(that: Any): Boolean = that.isInstanceOf[ConstNode]

    override def hashCode(): Int = if(value) 1 else 0

    override def equals(obj: Any): Boolean = canEqual(obj) && obj.asInstanceOf[ConstNode].value == value
}

object ConstNode {
    def unapply(arg: ConstNode): Option[Boolean] = Some(arg.value)
}
