package chatterbox

class RegexBot(entity: ChatEntity) extends ChatBot(entity) {

  object ParseState extends Enumeration {
    type ParseState = Value
    val Start, FindH, FindI, FoundHi, AfterHi, InWord, NullChar = Value
  }
  
  import ParseState._
  
  def query(input: String) {

    val s = "('T' + 't') 'h' 'a' 'n' 'k' ('s' + (' ' ' '* 'y' ('o') 'u'))"
    val ex = RegexParser.parse(s)

    entity.debug("regex model: "+ex)
    entity.debug("model mkstr: "+ex.mkStr)

  }
 
}
