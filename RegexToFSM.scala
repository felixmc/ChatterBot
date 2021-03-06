package chatterbox

object RegexToFSM {

  def parseND(ex: Expression): FiniteStateMachine = ex match {
    case Union(leftEx, rightEx) => {
      //println("union")

      val left = parseND(leftEx)
      val right = parseND(rightEx)

      val node = new Node()
      
      left.entryNode.edges.foreach(kv => {
        node.add(kv)
      })

      right.entryNode.edges.foreach(kv => {
        node.add(kv)
      })

      val fsm = new FiniteStateMachine(node)
      fsm.accepted.clear()
      
      fsm.entryNode.epsilons += (left.entryNode, right.entryNode)

      return fsm
    }
    case Symbol(c) => {
      //println("symbol")
      val node = new Node()

      val fsm = new FiniteStateMachine()
      fsm.entryNode.add(c, node)

      fsm.accepted.clear()
      fsm.accepted.add(node)

      return fsm
    }
    case Star(ex) => {
      //println("star")
      
      val rep = parseND(ex)
      val fsm = new FiniteStateMachine()

      rep.accepted.foreach(a => {
        //fsm.entryNode.edges.foreach(kv => {
        a.epsilons += fsm.entryNode
        //})
      })

      fsm.entryNode.epsilons += rep.entryNode

      return fsm
    }
    case Parens(ex) => {
      //println("parens")
      return parseND(ex)
    }
    case Concat(leftEx, rightEx) => {
      //println("concat")

      val left = parseND(leftEx)
      val right = parseND(rightEx)

      val fsm = new FiniteStateMachine(left.entryNode)
      
      left.accepted.foreach(a => {
        right.entryNode.edges.foreach(kv => {
          //a.add(kv)
          a.epsilons ++= kv._2
        })
      })

      fsm.accepted.clear()

      return left
    } 
  }

  def parse(ex: Expression): FiniteStateMachine = ex match {
    case Union(leftEx, rightEx) => {
      //println("union")

      val left = parse(leftEx)
      val right = parse(rightEx)

      val node = new Node()
      
      left.entryNode.edges.foreach(kv =>  {
        node.add(kv)
      })

      right.entryNode.edges.foreach(kv => {
        node.add(kv)
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
      fsm.entryNode.add(c, node)

      fsm.accepted.clear()
      fsm.accepted.add(node)

      return fsm
    }
    case Star(ex) => {
      //println("star")
      
      val fsm = parse(ex)
      
      fsm.accepted.foreach(a => {
        fsm.entryNode.edges.foreach(kv => {
          a.add(kv)
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
          a.add(kv)
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
