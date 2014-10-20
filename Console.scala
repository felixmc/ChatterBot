package chatterbox

object Console {
  val ESC = 27.toChar

  case class Color(code: Int) {
    override def toString = s"$ESC[${code}m" 
  }

  object Colors {
    val Red    = Color(31)
    val White  = Color(37)
    val Green  = Color(32)
    val Blue   = Color(36)
    val Gray   = Color(37)
    val Orange = Color(33)
  }

  def color(s: String, color: Color): String = {
    return color + s + Colors.White
  }

}
