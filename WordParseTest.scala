package chatterbox

import java.util.Scanner

object WordParserTest extends App {

  val scan = new Scanner(System.in)
  
  while (true) {
    print("\nenter something: ")
    val result = WordGrammarParser.parse(scan.nextLine)

    println(result)
    println(result.mkStr)

    // if (result.isInstanceOf[SimpleSentence]) {
    //   val sen = result.asInstanceOf[SimpleSentence]
    //   println("why do " + sen.noun.mkStr + " " + sen.verb.mkStr) 
  
    //   val noun     = sen.noun.noun.word
    //   val nounWord = WordDictionary.identifyWord(sen.noun.noun.word)
    //   val nounDef  = nounWord.getSynset().getGloss().split("[:;]")(0)
    //   println("did you know that a(n) " + nounWord.getLemma + " is a " + nounDef)
    // }


  }


}
