package chatterbox

class GreetBot(entity: ChatEntity) extends ChatBot(entity) {

  object CMatch {
    def isA(c: Char) = c.toLower == 'a'
    def isL(c: Char) = c.toLower == 'l'
    def isO(c: Char) = c.toLower == 'o'
	  def isH(c: Char) = c.toLower == 'h'
  	def isI(c: Char) = c.toLower == 'i'
    def isE(c: Char) = c.toLower == 'e'
    def isW(c: Char) = c.toLower == 'w'
    def isD(c: Char) = c.toLower == 'd'
    def isY(c: Char) = c.toLower == 'y'
    def isWord(c: Char) = c.isLetterOrDigit
  }
  
  object ParseState extends Enumeration {
    type ParseState = Value
    val FindGreet, InWord,
        FoundA, FoundAl, FoundAlo, FoundAloh, FoundAloha, AfterAloha,
        FoundH, FoundHi, AfterHi,
                FoundHe, FoundHel, FoundHell, FoundHello, AfterHello,
                FoundHo, FoundHow, FoundHowd, FoundHowdy, AfterHowdy
        = Value
  }
  
  import ParseState._
  
  def query(input: String) {
    val it = input.iterator
    var state: ParseState = FindGreet
    
    while (it.hasNext) {
      val c = it.next

      state = state match {
        case FindGreet => {
          if (CMatch.isH(c))
            FoundH
          else if (CMatch.isA(c))
            FoundA
          else if (!CMatch.isWord(c))
            FindGreet
          else
            InWord
        }
        case InWord => {
          if (CMatch.isWord(c))
            InWord
          else
            FindGreet
        }
        case FoundA => {
          if (CMatch.isL(c))
            FoundAl
          else if (!CMatch.isWord(c))
            FindGreet
          else
            InWord
        }
        case FoundAl => {
          if (CMatch.isO(c))
            FoundAlo
          else if (!CMatch.isWord(c))
            FindGreet
          else
            InWord
        }
        case FoundAlo => {
          if (CMatch.isH(c))
            FoundAloh
          else if (!CMatch.isWord(c))
            FindGreet
          else
            InWord
        }
        case FoundAloh => {
          if (CMatch.isA(c))
            FoundAloha
          else if (!CMatch.isWord(c))
            FindGreet
          else
            InWord
        }
        case FoundAloha => {
          if (!CMatch.isWord(c))
            AfterAloha
          else
            InWord
        }
        case FoundH => {
          if (CMatch.isI(c))
            FoundHi
          else if (CMatch.isO(c))
            FoundHo
          else if (CMatch.isE(c))
            FoundHe
          else if (!CMatch.isWord(c))
            FindGreet
          else
            InWord
        }
        case FoundHi => {
          if (!CMatch.isWord(c))
            AfterHi
          else
            InWord
        }
        case FoundHo => {
          if (CMatch.isW(c))
            FoundHow
          else if (!CMatch.isWord(c))
            FindGreet
          else
            InWord
        }
        case FoundHow => {
          if (CMatch.isD(c))
            FoundHowd
          else if (!CMatch.isWord(c))
            FindGreet
          else
            InWord
        }
        case FoundHowd => {
          if (CMatch.isY(c))
            FoundHowdy
          else if (!CMatch.isWord(c))
            FindGreet
          else
            InWord
        }
        case FoundHowdy => {
          if (!CMatch.isWord(c))
            AfterHowdy
          else
            InWord
        }
        case FoundHe => {
          if (CMatch.isL(c))
            FoundHel
          else if (!CMatch.isWord(c))
            FindGreet
          else
            InWord
        }
        case FoundHel => {
          if (CMatch.isL(c))
            FoundHell
          else if (!CMatch.isWord(c))
            FindGreet
          else
            InWord
        }
        case FoundHell => {
          if (CMatch.isO(c))
            FoundHello
          else if (!CMatch.isWord(c))
            FindGreet
          else
            InWord
        }
        case FoundHello => {
          if (!CMatch.isWord(c))
            AfterHello
          else
            InWord
        }

        case AfterAloha => AfterAloha
        case AfterHi    => AfterHi
        case AfterHello => AfterHello
        case AfterHowdy => AfterHowdy
      }
    }
   
    // check for acceptance states
    if (state == FoundHi || state == AfterHi) {
      entity.say("hello")
    } else if (state == FoundAloha || state == AfterAloha) {
      entity.say("aloha!")
    } else if (state == FoundHello || state == AfterHello) {
      entity.say("hey there")
    } else if (state == FoundHowdy || state == AfterHowdy) {
      entity.say("welcome to the ride, cowboy!")
    }
  }
 
}
