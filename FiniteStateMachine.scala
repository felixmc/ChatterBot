package chatterbox

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.mutable.Set
import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue

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

  def toDtm(): FiniteStateMachine = {
    this
    /*val source = this
    val target = new FiniteStateMachine
    val init   = new HashSet[Node]
    init += source.entryNode

    // target.addState(init)

    val queue  = new Queue[Set[Node]]
    queue += init

    val parsedStates = new HashSet[Node]
    parsedStates ++= init

    while (!queue.isEmpty) {
      val curSet = queue.dequeue()

       for (si <- 32 to 122) {
        val symbol = si.toChar

        val newStates = new HashSet[Node]

        for (cState <- curSet) {
          val nextStates = cState.get(symbol)

          for (nextState <- nextStates) {
            newStates += nextState
            if (source.accepted(contains($nextState)))
              target.accepted += $nextState
          }
        }

        if (!newStates.isEmpty) {
          //if newstateset not in target {
        
            queue += newStates
          //}

          //target machine add transition state, new state set, symbol

        }

      }

    }*/
  }
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

object FSMPrinter {

  def printFSM(fsm: FiniteStateMachine) {
    val knownStates: Map[Node, Char] = new HashMap
    var nextState = 'A'

    val printQueue: Queue[Node] = new Queue
    printQueue += fsm.entryNode

    def indent(level: Int): String = {
      (" " * level) + (if (level == 0) "" else "↳ ")
    }

    def getName(node: Node): Char = {
      if (!knownStates.contains(node)) {
        knownStates.put(node, nextState)
        nextState = (nextState + 1).toChar
        
        if (!printQueue.contains(node))
          printQueue += node
      }
      
      return knownStates.get(node).get
    }

    def parseState(node: Node) {
      val name = getName(node)
      if (fsm.accepted.contains(node))
        println(s"$name*")
      else
        println(s"$name")

      node.edges.foreach(kv => {
        kv._2.foreach(n => {
          println(s"  (${kv._1}) →  ${getName(n)}")
        })
      })

       node.epsilons.foreach(n => {
          println(s"  (ε) →  ${getName(n)}")
       })
    }

    while (!printQueue.isEmpty) {
      parseState(printQueue.head)
      printQueue.dequeue()
      println("\n")
    }
  }

}
