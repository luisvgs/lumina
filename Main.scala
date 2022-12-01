import scala.collection.mutable

case class Token(tpe: TokenType, text: String, startPos: Int)

enum TokenType:
  case Minus, Plus, True, False, Integer, EOF

class Lexer(input: String):

  private def error(msg: String): Unit =
    print(msg)

  def lex(): List[Token] = {
    val tokens = mutable.ArrayBuffer.empty[Token]
    var currentPos = 0

    while (currentPos < input.length) {
      val tokenStartPos = currentPos
      val lookahead = input(currentPos)

      lookahead match {
        case x if x.isWhitespace => currentPos += 1
        case '+' => {
          currentPos += 1
          tokens += Token(TokenType.Plus, lookahead.toString, tokenStartPos)
        }
        case '-' => {
          currentPos += 1
          tokens += Token(TokenType.Minus, lookahead.toString, tokenStartPos)
        }
        case x if x.isDigit => {
          var text = ""
          while (currentPos < input.length && input(currentPos).isDigit) {
            text += input(currentPos)
          }
          currentPos += 1
        }
        case x if x.isLetter =>
          var text = ""
          while (currentPos < input.length && input(currentPos).isLetter) {
            text += input(currentPos)
          }
          currentPos += 1
          val tpe = text match {
            case "true"  => TokenType.True
            case "false" => TokenType.False
          }
        case _ =>
          error(s"Unknown character '$lookahead' at position $currentPos")
      }
    }

    tokens += Token(TokenType.EOF, "<EOF>", currentPos) // special end marker
    tokens.toList
  }


@main def main(): Unit = {
  println("Hello there!!")
  // val lex = new Lexer("aaa").lex()
  // println(lex)
}

// println(Lexer("1").lex())
// @main def hello: Unit =
//   println("Hello world!")
//   println(msg)

// def msg = "I was compiled by Scala 3. :)"