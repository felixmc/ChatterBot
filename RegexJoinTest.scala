package chatterbox

object RegexJoinTest extends App {

  val patterns = List(
    "('H' + 'h') 'i'"
   ,"('H' + 'h') 'e' 'l' 'l' 'o'"
   ,"('H' + 'h') 'o' 'w' 'd' 'y'"
   ,"('A' + 'a') 'l' 'o' 'h' 'a'"
   ,"('T' + 't') 'h' 'a' 'n' 'k' ('s' + (' ' ' '* 'y' 'o' 'u'))"
  )

  val regex = patterns.foldLeft[Expression](Empty())((e, next) => Union(e,RegexParser.parse(next)).reduce)
  
  println("\nregex parse tree: " + regex)

  //val fms = RegexToFSM.toND()


  val input = "Thank  you"

  /*if (fsm.matches(input)) {
    // println(s"input `$input` matches pattern `$patt`")
  } else {
    // println(s"input `$input` matches NOT pattern `$patt`")
  }

  println()
  
  FSMPrinter.printFSM(fsm)
  */
}
