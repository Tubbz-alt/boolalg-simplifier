package com.github.natanbc.boolalg

class NegNode(val id: Int, val n: Node) extends Node {
    override def simplify(context: SimplificationContext, nested: Boolean): Node = n match {
        case AndNode(v) => context.addStep(this, "De Morgan", context.or(v.map(n => context.neg(n).simplify(context))))
        case OrNode(v) => context.addStep(this, "De Morgan", context.and(v.map(n => context.neg(n).simplify(context))))
        case NegNode(v) => context.addStep(this, "Double negation", v.simplify(context))
        case ConstNode(v) => context.addStep(this, "Constant negation", context.constant(!v))
        case _ if nested => this
        case _ => context.neg(n.simplify(context)).simplify(context, nested = true)
    }

    override def eval: Option[Boolean] = n.simplify(SimplificationContext.noop).eval.map(v => !v)

    override def toString: String = s"!$n"

    override def canEqual(that: Any): Boolean = that.isInstanceOf[NegNode]

    override def hashCode(): Int = n.hashCode()

    override def equals(obj: Any): Boolean = canEqual(obj) && obj.asInstanceOf[NegNode].n == n
}

object NegNode {
    def unapply(arg: NegNode): Option[Node] = Some(arg.n)
}
