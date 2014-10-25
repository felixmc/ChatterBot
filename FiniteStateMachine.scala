package chatterbox

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.mutable.Set
import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer

class FiniteStateMachine(start: Node = new Node()) {
  val entryNode: Node = start
  val accepted: Set[Node] = new HashSet()

  accepted.add(entryNode)

  def process(s: String): Node = process(s.iterator)

  def process(s: Iterator[Char]): Node = {
    var state = entryNode
    
    while (s.hasNext) {
      state = state.get(s.next)
    }
    
    return state
  }

  def matches(s: String): Boolean = accepted.contains(process(s))
}

class Node(var default: Node = ErrorNode) {
  val edges: Map[Char, List[Node]] = new HashMap
  val epsilons: ArrayBuffer[Node] = new ArrayBuffer

  def get(c: Char): Node = {
    val nodes = getAll(c)
    if (nodes.length == 0) default else nodes.head
  }
  
  def getAll(c: Char): List[Node] = {
    return edges.getOrElse(c, List())
  }
  
  def add(c: Char, node: Node) {
    edges.put(c, getAll(c) :+ node)
  }

  def add(kv: (Char, List[Node])) {
    edges.put(kv._1, getAll(kv._1) ++ kv._2)
  }
  
  override def toString(): String = ""
}

object ErrorNode extends Node {
  override def get(c: Char) = this
}

