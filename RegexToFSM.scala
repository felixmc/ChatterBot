package chatterbox

object RegexToFSM {

  def parse(ex: Expression): FiniteStateMachine = ex match {
    case Union(leftEx, rightEx) => {
      //println("union")

      val left = parse(leftEx)
      val right = parse(rightEx)

      val node = new Node()
      
      left.entryNode.edges.foreach(kv =>  {
        node.edges += kv
      })

      right.entryNode.edges.foreach(kv => {
        node.edges += kv
      })

      val fsm = new FiniteStateMachine(node)
    
      fsm.accepted.clear()
      
      left.accepted.foreach(a => {
        fsm.accepted.add(a)
      })
      
      right.accepted.foreach(a => {
        fsm.accepted.add(a)
      })

      return fsm
    }
    case Symbol(c) => {
      //println("symbol")
      val node = new Node()

      val fsm = new FiniteStateMachine()
      fsm.entryNode.edges.put(c, node)

      fsm.accepted.clear()
      fsm.accepted.add(node)

      return fsm
    }
    case Star(ex) => {
      //println("star")
      
      val fsm = parse(ex)
      
      fsm.accepted.foreach(a => {
        fsm.entryNode.edges.foreach(kv => {
          a.edges += kv
        })
      })

      fsm.accepted.add(fsm.entryNode)

      return fsm
    }
    case Parens(ex) => {
      //println("parens")
      return parse(ex)
    }
    case Concat(leftEx, rightEx) => {
      //println("concat")

      val left = parse(leftEx)
      val right = parse(rightEx)

      val fsm = new FiniteStateMachine(left.entryNode)
      
      left.accepted.foreach(a => {
        right.entryNode.edges.foreach(kv => {
          a.edges += kv
        })
      })

      fsm.accepted.clear()

      right.accepted.foreach(a => {
        fsm.accepted.add(a)
      })

      return fsm
    }

  }

}
