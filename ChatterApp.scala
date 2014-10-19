package chatterbox

import java.util.Scanner

object ChatterApp extends App {

  val iface: ChatInterface = new ChatTextInterface(System.in, System.out)
  val bots : BotFactory    = new BotFactory(iface.bot)
  val bot  : ChatBot       = bots.Greet
  val chat : ChatSession   = new ChatSession(bot, iface)

  chat.start()
}
