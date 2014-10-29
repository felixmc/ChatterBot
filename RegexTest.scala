package chatterbox

object RegexTest extends App {

  val patt = "('T' + 't') 'h' 'a' 'n' 'k' ('s' + (' ' ' '* 'y' 'o' 'u'))"

  val ex = RegexParser.parse(patt)
  println("regex string: " + patt + "\nregex model: "+ex + "\nregex model.mkStr: " + ex.mkStr)

  val fsm = RegexToFSM.parse(ex)
  println(fsm.entryNode.edges)

  val input = "Thank  you"

  if (fsm.matches(input)) {
    println(s"input `$input` matches pattern `$patt`")
  } else {
    println(s"input `$input` matches NOT pattern `$patt`")
  }

  println()
  
  FSMPrinter.printFSM(fsm)

}
