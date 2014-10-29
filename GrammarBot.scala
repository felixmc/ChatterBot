package chatterbox

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import scala.collection.mutable.HashSet
import scala.util.Random

class GrammarBot(entity: ChatEntity) extends ChatBot(entity) {
  val sent    = new ArrayBuffer[String]
  val history = new HashSet[Sentence]
  
  val greetings = List("hi", "hello", "howdy", "howdy parner", "hi there", "hello there", "what's up", "sup", "hey", "hey there", "aloha")
  val questions = List("Interesting. Tell me more", "Tell me something interesting", "What are your hobbies?", "How is your day going?", "How are you today?", "Can you believe this weather?", "You bore me.", "So what else is new?", "I'm gonna fall asleep", "I'm a little confused..")
  
  type Response = (Sentence) => String

  private def reverse(str: String): String = {
    var s = str
    s = s.replace("my", "YOUR")
    s = s.replace("your", "MY")
    s = s.replace("Im", "YOU'RE")
    s = s.replace("I", "YOU")
    s = s.replace("you", "me")
    return s.toLowerCase()
  }

  val askWhy: Response = s => {
    val sen  = s.asInstanceOf[SimpleSentence]
    val subj = sen.noun.noun.text
    val does = if (subj.takeRight(1) == "s" || subj == "you" || subj == "I") "do" else "does"
    
    val raw = "Why " + does + " " + sen.noun.mkStr + " " + sen.verb.mkStr + "?" 
    reverse(raw)
  }
 
  def definition(s: String): String = {
    //println("def subj: " + s)
    val wordDef = WordDictionary.getDef(s, Noun())
    
    val a = if (wordDef.startsWith("a ") || wordDef.startsWith("an ")) "" else if ("aeiou".indexOf(s.head) < 0) "a" else "an"

    "Did you know that " + a + " " + WordDictionary.getStem(s) + " is a " + wordDef
  }

  val question: Response = s => {
    questions(Random.nextInt(questions.size))
  }
  
  val didYouKnow: Response = s => {
    val sen  = s.asInstanceOf[SimpleSentence]
    var subj = sen.noun.noun.text
  
    if (subj == "I" || subj == "you") {
      if (sen.verb.isInstanceOf[ComplexVerbPhrase]) {
        subj = sen.verb.asInstanceOf[ComplexVerbPhrase].noun.noun.text
      }
    }

    if (subj == "I" || subj == "you") {
      question(s) 
    } else {
      definition(subj)
    }
  }
  
  val tellMeMore: Response = s => {
    val sen  = s.asInstanceOf[SimpleSentence]
    var subj = sen.noun.noun.text

    reverse("interesting. tell me more about " + subj).replace("you", "yourself")
  }

  val responses = List[Response](askWhy, didYouKnow, tellMeMore)

  def getReply(result: BotGrammar, retry: Int = 0): String = {
    val reply = if (result != null && result.isInstanceOf[Sentence] && retry < 5) {
      val sen = result.asInstanceOf[Sentence]
      history += sen

      result match {
        case g: GreetSentence => greetings(Random.nextInt(greetings.size))
        case s: SimpleSentence => responses(Random.nextInt(responses.size)).apply(sen)
      }
    } else if ((result != null && result.isInstanceOf[Sentence] && retry < 10) || (history.size > 0 && retry < 5)) {
      getReply(history.toList.apply(Random.nextInt(history.size)), retry + 1)
    } else {
      question(null) 
    }
    
    if (sent.contains(reply))
      if (retry < 10)
        return getReply(result, retry + 1)
      else return null
    else return reply
  }

  def query(input: String) {
    val result = WordGrammarParser.parse(input)
   
    println(" parse tree: "+result) 
    
    val reply = getReply(result)

    if (reply != null) {
      if (!sent.contains(reply)) {
        sent += reply
        entity.say(reply)
      }
    }
  }
 
}
