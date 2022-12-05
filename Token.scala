package Token

case class Token(tpe: TokenType, text: String, startPos: Int)

enum TokenType:
  case Minus, Plus, True, False, Integer, EOF
