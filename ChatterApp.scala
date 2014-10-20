package chatterbox

import java.util.Scanner

object ChatterApp extends App {

  val printDebug = false  || true

  val iface: ChatInterface = new ChatTextInterface(System.in, System.out, printDebug)
  val bots : BotFactory    = new BotFactory(iface.bot)
  val bot  : ChatBot       = bots.Regex
  val chat : ChatSession   = new ChatSession(bot, iface)

  chat.start()
}
