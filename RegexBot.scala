package chatterbox

import scala.collection.mutable.HashMap
import scala.util.Random

class RegexBot(entity: ChatEntity, phrases: Map[String, List[String]]) extends ChatBot(entity) {

  val patt = new HashMap[String, FiniteStateMachine]
 
  private def fsm(s: String): FiniteStateMachine = {
    if (!patt.contains(s))
      patt.put(s, RegexToFSM.parse(RegexParser.parse(s)))

    return patt.getOrElse(s, new FiniteStateMachine())
  }
  
  def query(input: String) {
    val options = phrases.find(p => {
      p._1 != "*" && fsm(p._1).matches(input)
    }) match {
      case Some(p) => p._2
      case None => phrases.getOrElse("*", List())
    }

    if (!options.isEmpty)
      entity.say(options(Random.nextInt(options.size)))
  }
 
}
