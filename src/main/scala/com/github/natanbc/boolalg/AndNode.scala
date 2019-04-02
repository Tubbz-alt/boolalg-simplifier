package com.github.natanbc.boolalg

import spray.json.{ JsArray, JsNumber, JsObject, JsString }

import scala.collection.mutable.ArrayBuffer

class AndNode(var id: Int, val values: Seq[Node]) extends Node {
  override def simplify(context: SimplificationContext, nested: Boolean): Node = {
    var v = values.map(_.simplify(context))
    if(v != values) {
      return new AndNode(id, v).simplify(context)
    }
    v = v.flatMap {
      case AndNode(n) => n
      case n => Some(n)
    }.distinct
    if(v != values) {
      return context.addStep(this, "Member flattening", context.and(v)).simplify(context, nested = true)
    }
    if(v.forall(_.eval.exists(identity))) {
      return context.addStep(this, "All inputs are true", context.constant(true))
    }
    if(v.exists(_.eval.exists(v => !v))) {
      return context.addStep(this, "One or more inputs are false", context.constant(false))
    }
    val normal = new ArrayBuffer[String]()
    val negated = new ArrayBuffer[String]()
    for(value <- v) {
      value match {
        case InputNode(n) =>
          if(negated.contains(n)) {
            return context.addStep(this, s"!$n * $n", context.constant(false))
          }
          normal += n
        case NegNode(InputNode(n)) =>
          if(normal.contains(n)) {
            return context.addStep(this, s"$n * !$n", context.constant(false))
          }
          negated += n
        case _ =>
      }
    }
    if(v.collectFirst {
      case OrNode(_) | ConstNode(true) => true
    }.isDefined) {
      return context.addStep(this, "Distributive", v.reduce[Node] {
        case (a: Node, b: ConstNode) => context.addStep(
          context.and(Seq(a, b)), "Multiplication by constant", if(b.value) {
            a
          } else {
            context.constant(false)
          }
        )
        case (a: ConstNode, b: Node) => context.addStep(
          context.and(Seq(a, b)), "Multiplication by constant", if(a.value) {
            b
          } else {
            context.constant(false)
          }
        )
        case (a: OrNode, b: OrNode) => context.or(
          crossProduct(a.values, b.values, (a, b) => context.and(Seq(a, b)))
        ) //.simplify(context)
        case (a: OrNode, b: Node) => context.or(
          crossProduct(a.values, Seq(b), (a, b) => context.and(Seq(a, b)))
        ) //.simplify(context)
        case (a: Node, b: OrNode) => context.or(
          crossProduct(Seq(a), b.values, (a, b) => context.and(Seq(a, b)))
        ) //.simplify(context)
        case (a, b) => context.and(Seq(a, b))
      }).simplify(context)
    }
    this
  }
  
  private def crossProduct(a: Seq[Node], b: Seq[Node], factory: (Node, Node) => Node): Seq[Node] = {
    for(va <- a; vb <- b) yield {
      factory(va, vb)
    }
  }
  
  override def eval: Option[Boolean] = {
    var result = true
    for(v <- values) {
      v.simplify(SimplificationContext.noop).eval match {
        case None => return None
        case Some(r) => result &&= r
      }
    }
    Some(result)
  }
  
  override def toJson: JsObject = JsObject(
    "type" -> JsString("AND"),
    "id" -> JsNumber(id),
    "values" -> JsArray(values.map(_.toJson):_*)
  )
  
  override def toString: String = values.mkString("(", " * ", ")")
  
  override def hashCode(): Int = values.hashCode()
  
  //sort ensures seq equality won't have false negatives
  override def equals(obj: Any): Boolean = {
    canEqual(obj) && obj.asInstanceOf[AndNode].values.sorted == values.sorted
  }
  
  override def canEqual(that: Any): Boolean = that.isInstanceOf[AndNode]
}

object AndNode {
  def unapply(arg: AndNode): Option[Seq[Node]] = Some(arg.values)
}
