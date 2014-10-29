package chatterbox

import scala.util.Random

class Lab5Bot(entity: ChatEntity) extends ChatBot(entity) {

  class RChar(val c: Char) {
    def is(s: Char) = c.toLower == s.toLower
    def isWord      = c.isLetterOrDigit
  }
  implicit def CharToRChar(c: Char) = new RChar(c)
  def rand(l: List[String]): String = l(Random.nextInt(l.size))

  object ParseState extends Enumeration {
    type ParseState = Value
    val Find, InWord,
        FoundA, FoundAl, FoundAlo, FoundAloh, FoundAloha, AfterAloha,
        FoundH, FoundHi, AfterHi,
                FoundHe, FoundHel, FoundHell, FoundHello, AfterHello,
                FoundHo, FoundHow, FoundHowd, FoundHowdy, AfterHowdy,
        FoundT, FoundTh, FoundTha, FoundThan, FoundThank, FoundThanks, FoundThankSP, FoundThankY, FoundThankYo, AfterThanks 
        = Value
  }
  
  import ParseState._
  
  def query(input: String) {
    val it = input.iterator
    var state: ParseState = Find
    
    while (it.hasNext) {
      val c = it.next

      state = state match {
        case Find => {
          if (c is 'h')
            FoundH
          else if (c is 'a')
            FoundA
          else if (c is 't')
            FoundT
          else if (!c.isWord)
            Find
          else
            InWord
        }
        case InWord => {
          if (c.isWord)
            InWord
          else
            Find
        }
        case FoundA => {
          if (c is 'l')
            FoundAl
          else if (!c.isWord)
            Find
          else
            InWord
        }
        case FoundAl => {
          if (c is 'o')
            FoundAlo
          else if (!c.isWord)
            Find
          else
            InWord
        }
        case FoundAlo => {
          if (c is 'h')
            FoundAloh
          else if (!c.isWord)
            Find
          else
            InWord
        }
        case FoundAloh => {
          if (c is 'a')
            FoundAloha
          else if (!c.isWord)
            Find
          else
            InWord
        }
        case FoundAloha => {
          if (!c.isWord)
            AfterAloha
          else
            InWord
        }
        case FoundH => {
          if (c is 'i')
            FoundHi
          else if (c is 'o')
            FoundHo
          else if (c is 'e')
            FoundHe
          else if (!c.isWord)
            Find
          else
            InWord
        }
        case FoundHi => {
          if (!c.isWord)
            AfterHi
          else
            InWord
        }
        case FoundHo => {
          if (c is 'w')
            FoundHow
          else if (!c.isWord)
            Find
          else
            InWord
        }
        case FoundHow => {
          if (c is 'd')
            FoundHowd
          else if (!c.isWord)
            Find
          else
            InWord
        }
        case FoundHowd => {
          if (c is 'y')
            FoundHowdy
          else if (!c.isWord)
            Find
          else
            InWord
        }
        case FoundHowdy => {
          if (!c.isWord)
            AfterHowdy
          else
            InWord
        }
        case FoundHe => {
          if (c is 'l')
            FoundHel
          else if (!c.isWord)
            Find
          else
            InWord
        }
        case FoundHel => {
          if (c is 'l')
            FoundHell
          else if (!c.isWord)
            Find
          else
            InWord
        }
        case FoundHell => {
          if (c is 'o')
            FoundHello
          else if (!c.isWord)
            Find
          else
            InWord
        }
        case FoundHello => {
          if (!c.isWord)
            AfterHello
          else
            InWord
        }
        case FoundT => {
          if (c is 'h')
            FoundTh
          else if (!c.isWord)
            Find
          else
            InWord
        }
        case FoundTh => {
          if (c is 'a')
            FoundTha
          else if (!c.isWord)
            Find
          else
            InWord
        }
        case FoundTha => {
          if (c is 'n')
            FoundThan
          else if (!c.isWord)
            Find
          else
            InWord
        }
        case FoundThan => {
          if (c is 'k')
            FoundThank
          else if (!c.isWord)
            Find
          else
            InWord
        }
        case FoundThank => {
          if (c is 's')
            FoundThanks
          else if (c is ' ')
            FoundThankSP
          else if (!c.isWord)
            Find
          else
            InWord
        }
        case FoundThanks => {
          if (!c.isWord)
            AfterThanks
          else
            InWord
        }
        case FoundThankSP => {
          if (c is ' ')
            FoundThankSP
          else if (c is 'y')
            FoundThankY
          else if (!c.isWord)
            Find
          else
            InWord
        }
        case FoundThankY => {
          if (c is 'o')
            FoundThankYo
          else if (!c.isWord)
            Find
          else
            InWord
        }
        case FoundThankYo => {
          if (c is 'u')
            FoundThanks
          else if (!c.isWord)
            Find
          else
            InWord
        }

        case AfterAloha  => AfterAloha
        case AfterHi     => AfterHi
        case AfterHello  => AfterHello
        case AfterHowdy  => AfterHowdy
        case AfterThanks => AfterThanks
      }
    } 
    
    val response = state match {
      case (FoundAloha | AfterAloha) => rand(List("Aloha", "Surf's up!"))
      case (FoundHi | AfterHi) => rand(List("Hi", "sup"))
      case (FoundHello | AfterHello) => rand(List("Hello", "How are you?"))
      case (FoundHowdy | AfterHowdy) => rand(List("Howdy partner"))
      case (FoundThanks | AfterThanks) => rand(List("You're welcome", "No, thank you"))
      case _ =>rand(List("What great wheather!", "Tell me about yourself", "What do you like to do?", "What makes you sad?"))
    }

    entity.say(response)
  }
 
}
