package chatterbox

class HiBot(entity: ChatEntity) extends ChatBot(entity) {

  object CMatch {
	  def isH(c: Char) = c.toLower == 'h'
    def isI(c: Char) = c.toLower == 'i'
    def isWord(c: Char) = c.isLetterOrDigit
	  def isBO(c: Char) = c == BO
    val BO = '\0'
  }
  
  object ParseState extends Enumeration {
    type ParseState = Value
    val Start, FindH, FindI, FoundHi, AfterHi, InWord, NullChar = Value
  }
  
  import ParseState._
  
  def query(input: String) {
    val it = input.iterator
    var state: ParseState = Start
    
    while (it.hasNext) {
      val c = it.next

      state = state match {
        case Start => {
          if (CMatch.isH(c))
            FindI
          else if (CMatch.isBO(c))
            NullChar
          else if (!CMatch.isWord(c))
            FindH
          else
            InWord
        }
        case InWord => {
          if (CMatch.isWord(c))
            InWord
          else
            FindH
        }
        case FindH => {
          if (CMatch.isH(c))
            FindI
          else if (!CMatch.isWord(c))
            FindH
          else
            InWord
        }
        case FindI => {
          if (CMatch.isI(c))
            FoundHi
          else if (!CMatch.isWord(c))
            FindH
          else
            InWord
        }
        case FoundHi => {
          if (CMatch.isWord(c))
            InWord
          else
            AfterHi
        }
        case AfterHi => AfterHi
      }
    }
   
    // check for acceptance states
    if (state == FoundHi || state == AfterHi) {
      entity.say("hello")
    }

  }
 
}
