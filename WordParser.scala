package chatterbox

object WordParser {

  def parse(s: String): List[String] = {
    return s.split("\\s+").toList.map(w => {
      // if first letter is caps, and the rest lower
      if (w.charAt(0).isUpper && (w.tail == w.tail.toLowerCase))
        w
      else w.toLowerCase
    })
  }


}
