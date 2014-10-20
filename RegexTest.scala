package chatterbox

object RegexTest extends App {

  val s = "('T' + 't') 'h' 'a' 'n' 'k' ('s' + (' ' ' '* 'y' ('o') 'u'))"
  //val s = "*'a'"

  val ex = RegexParser.parse(s)

  println(ex + "\n" + ex.mkStr)

}
