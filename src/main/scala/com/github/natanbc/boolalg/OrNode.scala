package com.github.natanbc.boolalg

import scala.collection.mutable.ArrayBuffer

class OrNode(val id: Int, val values: Seq[Node]) extends Node {
    override def simplify(context: SimplificationContext, nested: Boolean): Node = {
        val v = values.map(_.simplify(context)).flatMap {
            case OrNode(n) => n
            case n => Seq(n)
        }.distinct
        if(v != values && !nested) {
            return context.addStep(this, "Member simplification", context.or(v)).simplify(context, nested = true)
        }
        if(v.exists(_.eval.exists(identity))) {
            return context.addStep(this, "One or more inputs are true", context.constant(true))
        }
        if(v.forall(_.eval.exists(v => !v))) {
            return context.addStep(this, "All inputs are false", context.constant(false))
        }
        val normal = new ArrayBuffer[String]()
        val negated = new ArrayBuffer[String]()
        for(value <- v) {
            value match {
                case InputNode(n) =>
                    if(negated.contains(n)) {
                        return context.addStep(this, s"!$n + $n", context.constant(true))
                    }
                    normal += n
                case NegNode(InputNode(n)) =>
                    if(normal.contains(n)) {
                        return context.addStep(this, s"$n + !$n", context.constant(true))
                    }
                    negated += n
                case _ =>
            }
        }
        if(v.collectFirst { case ConstNode(false) => true }.isDefined) {
            return context.addStep(this, "Sum with 0", context.or(v.filter {
                case ConstNode(false) => false
                case _ => true
            })).simplify(context)
        }
        val filtered = v.filter {
            case AndNode(n) =>
                n.collectFirst {
                    case InputNode(name) if normal.contains(name) => true
                    case NegNode(InputNode(name)) if negated.contains(name) => true
                }.isEmpty
            case n: AndNode =>
                !v.filter(_ != n).exists(o => n.values.contains(o) || (o match {
                    case AndNode(p) => p.forall(n.values.contains)
                    case OrNode(p) => p.forall(n.values.contains)
                    case _ => false
                }))
            case _ => true
        }
        if(filtered != v) {
            return context.addStep(this, "Absorption", context.or(filtered))
        }
        this
    }

    override def eval: Option[Boolean] = {
        var result = true
        for(v <- values) {
            v.simplify(SimplificationContext.noop).eval match {
                case None => return None
                case Some(r) => result ||= r
            }
        }
        Some(result)
    }

    override def toString: String = values.mkString("(", " + ", ")")

    override def canEqual(that: Any): Boolean = that.isInstanceOf[OrNode]

    override def hashCode(): Int = values.hashCode()

    //sort ensures seq equality won't have false negatives
    override def equals(obj: Any): Boolean = canEqual(obj) && obj.asInstanceOf[OrNode].values.sorted == values.sorted
}

object OrNode {
    def unapply(arg: OrNode): Option[Seq[Node]] = Some(arg.values)
}
