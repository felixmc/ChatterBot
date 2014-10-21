package chatterbox

import scala.util.Random
import java.io.InputStream
import java.io.PrintStream
import java.util.Scanner

class ChatTextInterface(in: InputStream, out: PrintStream, printDebug: Boolean = false) extends ChatInterface {

  class ConsoleEntity(n: String, color: Console.Color = Console.Colors.White) extends ChatEntity(n) {
   
    var slowPrint  = false
    var printSpeed =  (120, 50)

    private val rand = new Random()

    def label = Console.color(name,color).toString
 
    def say(s: String) {
      prompt()
      if (slowPrint) {
        for (c <- s) { 
          out.print(c)
          Thread.sleep(rand.nextInt(printSpeed._1) + printSpeed._2)
        }
        out.println
      } else
        out.println(s)
    }

    override def debug(s: String) = if (printDebug) debugE.say(s) else {}

    def prompt() = {
      val padding = " " * (List(human, bot, system).map(x => x.name.length).max - name.length)
      out.print(s" $padding$label: ")
    }
  }

  out.println()

  val scanner = new Scanner(in)

  val human  = new ConsoleEntity("You", Console.Colors.Blue)
  val bot    = new ConsoleEntity("Bot", Console.Colors.Red)
  val system = new ConsoleEntity("System", Console.Colors.Green)
  val debugE = new ConsoleEntity("Debug", Console.Colors.Orange)

  bot.slowPrint     = true
  system.slowPrint  = true
  system.printSpeed = (100,0)

  def getInput = {
    human.prompt
    scanner.nextLine
  }

}

