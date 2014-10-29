package chatterbox

import scala.collection.mutable.HashMap
import edu.mit.jwi._
import edu.mit.jwi.item._
import edu.mit.jwi.morph._
import java.net.URL

abstract class PartOfSpeech

case class Verb() extends PartOfSpeech
case class Noun() extends PartOfSpeech
case class Adjective() extends PartOfSpeech
case class Pronoun() extends PartOfSpeech
case class Preposition() extends PartOfSpeech
case class Article() extends PartOfSpeech
case class Greeting() extends PartOfSpeech
case class Interogative() extends PartOfSpeech

object WordDictionary {

  val wnhome = "/home/felix/Development/libs/WordNet-3.1/"
  val url = new URL("file", null, wnhome) 
  
  val dict = new Dictionary(url)
  dict.open()
  
  lazy val iwordCache = new HashMap[String, IWord]
  lazy val stemmer = new WordnetStemmer(dict)

  val wordData: Map[PartOfSpeech, List[String]] = Map(
    (Preposition(),      List("about","above","across","after","against","along","alongside","amid","among","amongst","around","as","aside","at","before","behind","below","beneath","beside","besides","between","beyond","but","by","despite","during","except","excluding","for","from","in","including","inside","into","near","next","of","off","on","onto","out","over","per","regarding","since","than","through","throughout","till","to","toward","towards","under","underneath","until","unto","upon","versus","via","vice","with","within","without","worth"))
   ,(Pronoun(),  List("I", "you", "me", "they", "he", "she", "him", "her", "it", "them", "we", "us", "this", "these", "that", "those", "myself", "yourself"))
   ,(Article(),      List("a", "an", "the", "some", "all", "few", "many", "one", "my", "your", "their", "our", "her", "his"))
   ,(Greeting(),     List("hello", "hi", "greetings", "aloha", "howdy", "hey"))
   ,(Interogative(), List("who","whom","what","which","whose","where","why","how"))
  )

  def guessPOS(word: String, hint: PartOfSpeech = null): PartOfSpeech = {
    return wordData.keys.find(p => wordData(p).contains(word)) match {
      case None => {
        val aIdx = dict.getIndexWord(word, POS.ADJECTIVE)
        val nIdx = dict.getIndexWord(word, POS.NOUN)
        val vIdx = dict.getIndexWord(word, POS.VERB)
     
        (aIdx, nIdx, vIdx) match {
          case (a: IndexWord, null, null) => Adjective()
          case (null, n: IndexWord, null) => Noun()
          case (null, null, v: IndexWord) => Verb()

          case (a: IndexWord, n: IndexWord, null) => if (hint == Noun()) Noun() else Adjective()
          case (a: IndexWord, null, v: IndexWord) => if (hint == Verb()) Verb() else Adjective()
          case (null, n: IndexWord, v: IndexWord) => if (hint == Verb()) Verb() else Noun()

          case (a: IndexWord, n: IndexWord, v: IndexWord) => hint match {
            case Noun() => Noun()
            case Adjective() => Adjective()
            case Verb() => Verb()
            case _ => Noun()
          }
          case (null, null, null) => null
        }
      }
      case Some(pos) => pos
    }
  }

  def canItBe(w: String, part: PartOfSpeech): Boolean = {
    if (wordData.contains(part)) {
      return wordData.get(part).contains(w)
    } else { 
      val word = getStem(w, part)
      val widx = part match {
        case Noun() => dict.getIndexWord(word, POS.NOUN)
        case Verb() => dict.getIndexWord(word, POS.VERB)
        case Adjective() => dict.getIndexWord(word, POS.ADJECTIVE)
        case _ => null
      }

      return widx != null
    }
  }
  
  def getDef(word: String, _pos: PartOfSpeech = null): String = {
    val pos: POS = _pos match {
      case Noun() => POS.NOUN
      case Verb() => POS.VERB
      case Adjective() => POS.ADJECTIVE
      case _ => null
    }

    val stem = getStem(word, _pos)
    if (stem != null) {
      val wIdx = dict.getIndexWord(stem, pos)
      if (wIdx != null) {
        return dict.getWord(wIdx.getWordIDs().get(0)).getSynset().getGloss.split("[:;]")(0)
      } else return null
    } else return null
  }

  def getStem(word: String, part: PartOfSpeech = null): String = {
    val pos = part match {
      case Noun() => POS.NOUN
      case Verb() => POS.VERB
      case Adjective() => POS.ADJECTIVE
      case _ => null
    }

    var stems = stemmer.findStems(word, pos)
    if (!stems.isEmpty) return stems.get(0)
    else return null
  }

  // def identifyWord(word: String, prev: WordGrammar = EmptyGrammar): IWord = {
  //   // val followsNoun = prev.isInstanceOf[Noun] //&& word.endWith("s")
    
  //   if (wordCache.contains(word)) return wordCache.get(word).get

  //   var stems = stemmer.findStems(word, null)
  //   if (!stems.isEmpty) {
  //     val stem = stems.get(0)
  //     if (wordCache.contains(stem)) return wordCache.get(stem).get
        
  //     val wordIdx = findIndex(stem, prev)

  //     if (wordIdx != null) {
  //       val iword = dict.getWord(wordIdx.getWordIDs().get(0))
  //       wordCache.put(word, iword)
  //       wordCache.put(stem, iword)
  //       return iword
  //     }
  //   } 
    
  //   return null
  // }

  // def findIndex(word: String, prev: WordGrammar): IndexWord = {
  //   val nounIdx = dict.getIndexWord(word, POS.NOUN)
  //   val verbIdx = dict.getIndexWord(word, POS.VERB)
   
  //   val forceVerb = prev.isInstanceOf[Noun] || prev.isInstanceOf[Interogative]

  //   if (word == "what") {
  //     println("f: " + forceVerb)
  //     println("n: "+nounIdx.getTagSenseCount)
  //     println("v: "+verbIdx.getTagSenseCount)
  //   }

  //   return (nounIdx, verbIdx) match {
  //     case (null, v: IndexWord) => v
  //     case (n: IndexWord, null) => n
  //     case (n: IndexWord, v: IndexWord) => if (forceVerb) v else n //if (n.getTagSenseCount > v.getTagSenseCount) n else v
  //     case (null, null) => null
  //   }
  // }

}
