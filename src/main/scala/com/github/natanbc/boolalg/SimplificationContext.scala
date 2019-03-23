package com.github.natanbc.boolalg

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class SimplificationContext private(parent: SimplificationContext) {
    def this() = this(null)

    val steps = new ArrayBuffer[(Node, String, Node)]()
    val nodes = new mutable.HashMap[Int, Node]()
    private var _nextId: Int = 0

    private def nextId: Int = {
        _nextId = _nextId + 1
        _nextId
    }

    def addStep(original: Node, reason: String, result: Node): Node = {
        steps += ((original, reason, result))
        result
    }

    def newContext(): SimplificationContext = {
        val c = new SimplificationContext(this)
        c._nextId = _nextId
        c
    }

    def copyToParent(): Unit = {
        if(parent == null) {
            throw new IllegalArgumentException("This context has no parent!")
        }
        parent.steps.appendAll(steps)
        parent._nextId = math.max(parent._nextId, _nextId)
    }

    def and(values: Seq[Node]): Node = save(values match {
        case Seq() => throw new IllegalArgumentException("Cannot build AND node with no children")
        case Seq(a) => a
        case other => new AndNode(nextId, other)
    })

    def constant(i: Int): Node = constant(i match {
        case 0 => false
        case 1 => true
        case other => throw new IllegalArgumentException(s"Value for constants must be either 0 or 1! Provided value: $other")
    })
    def constant(v: Boolean): Node = save(new ConstNode(nextId, v))

    def input(name: String): Node = save(new InputNode(nextId, name))

    def neg(n: Node): Node = save(new NegNode(nextId, n))

    def or(values: Seq[Node]): Node = save(values match {
        case Seq() => throw new IllegalArgumentException("Cannot build OR node with no children")
        case Seq(a) => a
        case other => new OrNode(nextId, other)
    })

    private def save(n: Node): Node = {
        nodes.put(n.id, n)
        n
    }
}

object SimplificationContext {
    val noop: SimplificationContext = new SimplificationContext {
        override def addStep(original: Node, reason: String, result: Node): Node = result
    }
}
