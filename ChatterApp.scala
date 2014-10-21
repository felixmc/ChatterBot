package chatterbox

import java.util.Scanner

object ChatterApp extends App {

  val printDebug = false || true

  val logic = List(
    ("('H' + 'h') 'i'", List("Hi", "sup")),
    ("('H' + 'h') 'e' 'l' 'l' 'o'", List("Hello", "How are you?")),
    ("('H' + 'h') 'o' 'w' 'd' 'y'", List("Howdy partner")),
    ("('A' + 'a') 'l' 'o' 'h' 'a'", List("Aloha", "Surf's up!")),
    ("('T' + 't') 'h' 'a' 'n' 'k' ('s' + (' ' ' '* 'y' 'o' 'u'))", List("You're welcome", "No, thank you")),
    ("*", List("What great wheather!", "Tell me about yourself", "What do you like to do?", "What makes you sad?"))
  ).toMap

  val iface: ChatInterface = new ChatTextInterface(System.in, System.out, printDebug)
  val bots : BotFactory    = new BotFactory(iface.bot)
  val bot  : ChatBot       = bots.Regex(logic)
  val chat : ChatSession   = new ChatSession(bot, iface)

  chat.start()
}
