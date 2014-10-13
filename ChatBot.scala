package chatterbox

import java.lang.Runnable

abstract class ChatBot(val in: BotReader, val out: BotWriter) extends Runnable {
  def name: String = this.getClass.getSimpleName
  def read(in: String)
}
