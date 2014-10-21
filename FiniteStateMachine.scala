package chatterbox

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.mutable.Set
import scala.collection.mutable.HashSet

class FiniteStateMachine(start: Node = new Node()) {
  val entryNode: Node = start
  val accepted: Set[Node] = new HashSet()

  accepted.add(entryNode)

  def process(s: String): Node = process(s.iterator)

  def process(s: Iterator[Char]): Node = {
    var state = entryNode
    
    while (s.hasNext) {
      state = state.process(s.next)
    }
    
    return state
  }

  def matches(s: String): Boolean = accepted.contains(process(s))
}

class Node(var default: Node = ErrorNode) {
  val edges: Map[Char, Node] = new HashMap
  def process(c: Char): Node = edges.getOrElse(c, default)
  override def toString(): String = ""
}

object ErrorNode extends Node {
  override def process(c: Char) = this
}

