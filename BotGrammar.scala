package chatterbox

abstract class BotGrammar {
  def mkStr: String
}

object BadGrammar extends BotGrammar {
  def mkStr = "[error]"
}

object EmptyGrammar extends BotGrammar {
  override def toString = "EmptyGrammar()"
  def mkStr = "[empty]"
}

abstract class Word(val text: String) extends BotGrammar {
  def mkStr: String = text
}

case class UnknownWG(s: String) extends Word(s)
case class PrepositionWG(s: String) extends Word(s)
case class GreetingWG(s: String) extends Word(s)
case class VerbWG(s: String) extends Word(s)
case class AdjectiveWG(s: String) extends Word(s)
case class ArticleWG(s: String) extends Word(s)
case class NounWG(s: String) extends Word(s)
//case class PronounWG(s: String) extends Noun(s)

abstract class Nominal(val noun: NounWG) extends BotGrammar { def mkStr: String = noun.mkStr }
case class SimpleNominal(override val noun: NounWG) extends Nominal(noun)
case class AdjectiveNominal(adj: AdjectiveWG, nominal: Nominal) extends Nominal(nominal.noun) { override def mkStr: String = adj.mkStr + " " + noun.mkStr }


abstract class NounPhrase(val noun: NounWG) extends BotGrammar
case class SimpleNounPhrase(nom: Nominal) extends NounPhrase(nom.noun) { def mkStr: String = nom.mkStr }
case class ArticleNounPhrase(article: ArticleWG, nom: Nominal) extends NounPhrase(nom.noun) { def mkStr: String = article.mkStr + " " + nom.mkStr }
case class PrepNounPhrase(prep: PrepositionWG, np: NounPhrase) extends NounPhrase(np.noun) { def mkStr: String = prep.mkStr + " " + np.mkStr }


abstract class VerbPhrase(val verb: VerbWG) extends BotGrammar
case class SimpleVerbPhrase(override val verb: VerbWG) extends VerbPhrase(verb) { def mkStr: String = verb.mkStr }
case class ComplexVerbPhrase(override val verb: VerbWG, noun: NounPhrase) extends VerbPhrase(verb) { def mkStr: String = verb.mkStr + " " + noun.mkStr }


abstract class Sentence extends BotGrammar

case class GreetSentence(greet: GreetingWG) extends Sentence {
  def mkStr: String = greet.mkStr
}

case class SimpleSentence(noun: NounPhrase, verb: VerbPhrase) extends Sentence {
  def mkStr: String = noun.mkStr + " " + verb.mkStr
}

case class AddRouteRequest(city1: String, city2: String, distance: Float) extends Sentence {
  def mkStr: String = city1 + " and " + city2 + " are connected by a" + distance + " km road"
}

case class FindRouteRequest() extends Sentence {
  def mkStr: String = "What is the shortest route for those cities"
}

// case class Question(inter: Interogative, verb1: ComplexVerbPhrase, verb2: ComplexVerbPhrase) extends Sentence {
//   def mkStr: String = inter.mkStr + " " + verb1.mkStr + " " + verb2.mkStr + "?"
// }


