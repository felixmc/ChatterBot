package chatterbox

import java.io.OutputStream
import java.io.PrintWriter

class BotWriter(out: OutputStream) {

  private val writer = new PrintWriter(out)

  def write(s: String, name: String = "Bot") {
    writer.println(Console.color(name, Console.Colors.Red) + ": " + s)
    writer.flush()
  }

}
