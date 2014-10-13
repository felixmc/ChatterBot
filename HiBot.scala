package chatterbox

class HiBot(out: BotWriter) extends ChatBot(out) {

  object CMatch {
	  def isH(c: Char) = c == 'h' || c == 'H'
  	def isI(c: Char) = c == 'i' || c == 'I'
    def isSp(c: Char) = c == ' '
    def isWord(c: Char) = c.isLetterOrDigit
	  def isBO(c: Char) = c == BO
    val BO = '\0'
  }
  
  object ParseState extends Enumeration {
    type ParseState = Value
    val Intro, FindH, FindI, FoundHi, AfterHi, InWord, NullChar = Value
  }
  
  import ParseState._
  
  def read(in: String) {
    val it   = in.iterator
    def next = if (it.hasNext) it.next else CMatch.BO
    
    var state : ParseState = Intro
    var c = next
    
    while (!CMatch.isBO(c)) {
      state = state match {
        case Intro => {
          if (CMatch.isH(c))
            FindI
          else if (CMatch.isSp(c))
            FindH
          else if (CMatch.isBO(c))
            NullChar
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
     
      c = next
    }
   
    // check for acceptance states
    if (state == FoundHi || state == AfterHi) {
      out.println("hello", name)
    }

  }
    
}
