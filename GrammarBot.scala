package chatterbox

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
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

  val cities = new HashSet[String]
  val roads  = new HashSet[Route]

  case class Route(city1: String, city2: String) {
    var dist: Double = 0
    def contains(city: String): Boolean = city1 == city || city2 == city
    def other(city: String): String = if (city1 == city) city2 else city1
  }

  def calcRoute(): List[Route] = {
    
    // add single edges to it by default
    val routes = new HashSet[List[Route]]

    // try start from every city
    for (city <- cities) {
      routes ++= traverseRoute(city)
    }

    // traverse all solutions and find smallest
    if (routes.isEmpty)
      return List()
    else
      routes.foldLeft[List[Route]](List())((best, path) => {
        val bestDist = best.foldLeft[Double](0)((sum, route) => route.dist + sum)
        val pathDist = path.foldLeft[Double](0)((sum, route) => route.dist + sum)
        if (best.isEmpty || pathDist < bestDist)
          path
        else best
      })
  }

  def getEdges(city: String): HashSet[Route] = roads.filter(r => {
    //println("city "+ city + " matches " + r + " => " + r.contains(city))
    r.contains(city)
  })

  def pathContains(path: HashSet[Route], city: String) = path.exists(r => r.contains(city))

  def isSolution(path: List[Route]): Boolean = {
    val foundCities = path.foldLeft(new HashSet[String])((set, r) => {
      set += r.city1
      set += r.city2
    })

    // if has all cities
    return foundCities.size == cities.size
  }

  def traverseRoute(city: String, path: List[Route] = List()): HashSet[List[Route]] = {

    // if has all cities
    if (isSolution(path)) {
      return new HashSet[List[Route]] += path
    } else if (city == null) { // if you run out of cities, then give up on solution
      return null
    } else { // else attempt
      //println("\ncity: " + city)
      val edges = getEdges(city)
      //println(edges)
      val solutions = new HashSet[List[Route]]
      
      for (edge <- edges) {
        //println("\tedge: " + edge)
        if (!path.contains(edge)) {
           val other = edge.other(city)
           val newPath = path :+ edge

           val trav = traverseRoute(other, newPath)

           if (trav != null)
             solutions ++= trav
        }
      }

      return solutions
    }
  }

  def query(input: String) {
    val result = WordGrammarParser.parse(input)
    println(" parse tree: "+result) 
    println(roads)
    println(cities)

    result match {
      case AddRouteRequest(c1,c2,d) => {
        cities ++= List(c1, c2)
        val route = Route(c1,c2)
        route.dist = d
        roads += route
        entity.say("Okay.")
      }
      case FindRouteRequest() => entity.say("The shortest route is " + calcRoute())
      case _ => {
        val reply = getReply(result)

        if (reply != null) {
          if (!sent.contains(reply)) {
            sent += reply
            entity.say(reply)
          }
        }
      }
    }

  }
 
}
