package chatterbox

import scala.collection.mutable.Stack
import edu.mit.jwi.item._

class WordIterator(val words: List[String]) {
  private var i = 0

  def pos  = i
  def word = wordAt(i)
  def wordAt(ni: Int) = if (ni < words.length) words(ni) else null 

  def peek = wordAt(i)
  def peek(ni: Int) = wordAt(i + ni)

  def next: String = {
    val w = word
    i = i + 1
    return w
  }

  def length  = words.length
  def hasNext = i < words.length
}

class GrammarParseException(sp: WordIterator, exp: PartOfSpeech) extends Exception(s"Unexpected word '${sp.word}' at position ${sp.pos} was expecting " + exp)

object WordGrammarParser {

  def parse(s: String): BotGrammar = parse(new WordIterator(WordParser.parse(s)))

  def parse(words: WordIterator): BotGrammar = {
    return try {
      parseSentence(words)
    } catch {
      case gpe: GrammarParseException => {
        println(gpe)
        BadGrammar
      }
      case e: Exception => BadGrammar
    }
  }

  def parseSentence(words: WordIterator): Sentence = {
    val stack = new Stack[BotGrammar]
    val word = words.word
    //stack.push(word)

    return if (words.peek(1) == "and" && words.peek(3) == "are" && words.peek(4) == "connected" && words.peek(5) == "by" && words.peek(6) == "a" && words.peek(8) == "km" && words.peek(9) == "road" && words.length == 11) {
      AddRouteRequest(words.peek(0), words.peek(2), words.peek(7).toDouble)
    } else if (words.peek(0) == "what" && words.peek(1) == "is" && words.peek(2) == "the" && words.peek(3) == "shortest" && words.peek(4) == "route" && words.peek(5) == "for" && words.peek(6) == "those" && words.peek(7) == "cities") {
      FindRouteRequest()
    } else WordDictionary.guessPOS(word) match {
      case Greeting() => GreetSentence(GreetingWG(words.next))
      case _ => SimpleSentence(parseNP(words,stack), parseVP(words,stack))
    }
  }

  def parseNP(words: WordIterator, stack: Stack[BotGrammar]): NounPhrase = {
    val word = words.word
    
    return WordDictionary.guessPOS(word) match {
      case Article() => ArticleNounPhrase(ArticleWG(words.next), parseNominal(words, stack))
      case Preposition() => PrepNounPhrase(PrepositionWG(words.next), parseNP(words, stack))
      case _ => SimpleNounPhrase(parseNominal(words,stack))
    }
  }
  
  def parseNominal(words: WordIterator, stack: Stack[BotGrammar]): Nominal = {
    val word = words.word

    return WordDictionary.guessPOS(word) match {
      case Adjective() => AdjectiveNominal(AdjectiveWG(words.next), parseNominal(words, stack))
      case _ => SimpleNominal(parseNoun(words,stack))
    }
  }
  
  def parseNoun(words: WordIterator, stack: Stack[BotGrammar]): NounWG = {
    val word = words.word

    return WordDictionary.guessPOS(word) match {
      case Pronoun() => NounWG(words.next)
      case _ => if (WordDictionary.canItBe(word, Noun()))
        NounWG(words.next)
      else throw new GrammarParseException(words, Noun())
    }
  }

  def parseVP(words: WordIterator, stack:Stack[BotGrammar]): VerbPhrase = {
    val verb = if (WordDictionary.canItBe(words.word, Verb()))
        VerbWG(words.next)
      else throw new GrammarParseException(words, Verb())
    
    if (words.hasNext) {
       return ComplexVerbPhrase(verb, parseNP(words,stack)) 
    } else {
      return SimpleVerbPhrase(verb)
    }
  }

  /*def parse(s: String): Stack[WordGrammar] = {
    val stack = new Stack[WordGrammar]
    var last: WordGrammar = EmptyGrammar
    val words = WordParser.parse(s).map(w => {
      last = parseWord(w, last)
      last
    })

    var i = 0
    def peek: WordGrammar = if (i+1 < words.length) words(i+1) else EmptyGrammar

    while (i < words.length) {
      // add new word to stack -- shift operation
      stack.push( words(i) )

      do {
        println(peek + "  " + stack)
      // try reduce stack until it no longer can be reduced
      } while (reduce(stack, peek))

      i = i + 1
    }
    
    println("\"" + s + "\" IS SENTENCE: " + (stack.top.isInstanceOf[Sentence]))
    println("\n")

    return stack
  }



  def parseWord(text: String, last: WordGrammar): WordGrammar =
    if (greetings.contains(text.toLowerCase))
      Greeting(text)
    else if (pronouns.contains(text.toLowerCase))
      Pronoun(text)
    else if (interogatives.contains(text.toLowerCase))
      Interogative(text)
    else if (prepositions.contains(text.toLowerCase))
      Preposition(text)
    else if (articles.contains(text.toLowerCase))
      Article(text)
    else if (text.charAt(0).isUpper)
      ProperNoun(text)
    else { // check for verbs and nouns
      val word = wordDb.identifyWord(text, last)
      if (word != null) {
        word.getPOS match {
          case POS.VERB => Verb(text, word.getLemma)
          case POS.NOUN => CommonNoun(text, word.getLemma)
          case _ => Ambiguous(text, word.getLemma)
        }
      } else Unknown(text)
    }

  def reduce(stack: Stack[WordGrammar], peek: WordGrammar): Boolean = {
    var changed = true
    
    def s (i: Int): WordGrammar = if (!stack.isEmpty && i < stack.length) stack(i) else EmptyGrammar 
    def get[G <: WordGrammar]: G = stack.pop.asInstanceOf[G]

    // if (is[VerbPhrase](0) && is[NounPhrase](1)) {
    if (s(0).isInstanceOf[Noun]
      && s(1).isInstanceOf[Article]
      && !peek.isInstanceOf[Preposition]) { // article noun
      val noun    = get[Noun]
      val article = get[Article]
      stack.push(ArticleNoun(article, noun))
    } else if (s(0).isInstanceOf[NounPhrase]
      && s(1).isInstanceOf[Preposition]
      && s(2).isInstanceOf[NounPhrase]
      && s(3).isInstanceOf[Article]) { // long noun
      val article = get[Article]
      val pre     = get[NounPhrase]
      val prep    = get[Preposition]
      val post    = get[NounPhrase]
      stack.push(ArticleNounPreposition(article, pre, prep, post))
    } else if (s(1).isInstanceOf[Verb]
      && s(0).isInstanceOf[NounPhrase]) { // complex verb
      val noun = get[NounPhrase]
      val verb = get[Verb]
      stack.push(ComplexVerbPhrase(verb, noun))
    } else if (s(0).isInstanceOf[Verb]
      && !peek.isInstanceOf[Noun]
      && !peek.isInstanceOf[Article]) { // simple verb
      val verb = get[Verb]
      stack.push(SimpleVerbPhrase(verb))
    } else if (s(0).isInstanceOf[Noun]) { // simple noun
      val noun    = get[Noun]
      stack.push(SimpleNoun(noun))
    } else if (s(0).isInstanceOf[ComplexVerbPhrase]
      && s(1).isInstanceOf[ComplexVerbPhrase]
      && s(2).isInstanceOf[Interogative]) {
        val verb1 = get[ComplexVerbPhrase]
        val verb2 = get[ComplexVerbPhrase]
        val inter = get[Interogative]
        stack.push(Question(inter, verb1, verb2))
    } else if (s(0).isInstanceOf[VerbPhrase]
      && s(1).isInstanceOf[NounPhrase] && peek == EmptyGrammar) { // simple sentence
      val verb = get[VerbPhrase]
      val noun = get[NounPhrase]
      stack.push(SimpleSentence(noun, verb))
    } else {
      changed = false
    }

    return changed
  }*/


}
