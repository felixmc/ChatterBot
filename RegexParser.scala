package chatterbox

object RegexParser {

  object CMatch {
    def isStar(c: Char)   = c == '*'
    def isAlph(c: Char) = c.isLetterOrDigit || c == ' '
    def isUnion(c: Char)  = c == '+'
  }

  object ParseState extends Enumeration {
    type ParseState = Value
    val InWord = Value
  }

  def parse(s: String): Expression = parse(new RegexStringIterator(s))

  def parse(s: RegexStringIterator): Expression = {
    val ex = parseExpression(s)

    if (s.hasNext)
      throw new RegexParseException(s)
    else
      return ex.reduce
  }

  def parseExpression(s: RegexStringIterator): Expression = {
    
    var ex = s.c match {
      case '\'' => parseSymbol(s)
      case '('  => parseParens(s)
      case '\0' => Empty()
      case _ => Empty()
    }

    if (ex != Empty()) {
      if (s.c == '*') {
        s.next
        ex = Star(ex)
      }
    
      ex = if (s.c == '\0') {
        ex
      } else {
        Concat(ex, parseExpression(s))
      }
    }

    return ex
  }

  def parseSymbol(s: RegexStringIterator): Expression = {
    if (s.c == '\'') s.nextWs else throw new RegexParseException(s)
    val ex = if (CMatch.isAlph(s.c)) {
      val sc = s.c
      s.next
      Symbol(sc)
    } else throw new RegexParseException(s)
    if (s.c == '\'') {
      s.next
      return ex
    } else throw new RegexParseException(s)
  }

  def parseParens(s: RegexStringIterator): Expression = {
    if (s.c == '(') s.next else throw new RegexParseException(s)
    var ex = parseExpression(s)
    if (s.c == '+') {
      s.next 
      ex = Union(ex, parseExpression(s))
    } else {
      ex = Parens(ex)
    }
    if (s.c == ')') {
      s.next
      return ex
    } else throw new RegexParseException(s)
  }

}

class RegexParseException(sp: RegexStringIterator) extends Exception(s"Unexpected char '${sp.c}' at position ${sp.pos}")

class RegexStringIterator(val s: String) {
  private var i = 0

  def pos = i
  def c   = cAt(i)
  def cAt(ni: Int) = if (ni < s.length) s.charAt(ni) else '\0' 

  def peekI: Int = {
    var ni = i + 1
    while (cAt(ni).isWhitespace) ni = ni + 1
    return ni 
  }

  def peek = cAt(peekI)
  
  def next: Char = {
    i = peekI
    return c
  }

  def nextWs: Char = {
    i = i + 1
    return c
  }
  
  def hasNext = peekI < s.length

}
