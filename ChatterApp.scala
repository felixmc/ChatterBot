package chatterbox

import java.util.Scanner

object ChatterApp extends App {

  println("\nChatterBot v0.1")
  println("(q to quit)\n")
  
  val scan = new Scanner(System.in)
  val writer = new BotWriter(System.out)
  
  //val bot: ChatBot = new HiBot(writer)
  val bot: ChatBot = new GreetingBot(writer)

  var res = ""

  do {
    print(Console.color("You", Console.Colors.Blue) + ": ")
    res = scan.nextLine()
    bot.query(res)
  } while (!res.equals("q"))

  println
  writer.println("Goodbye!")
  println
  
}
