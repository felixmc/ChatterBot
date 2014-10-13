package chatterbox

import java.io.InputStream
import java.io.Scanner

class BotReader(in: InputStream) {
  val scanner = new Scanner(in)

  def read(): String = {
    while (scanner.hasNextLine()) {
      yield scanner.nextLine()
    }
  }

}
