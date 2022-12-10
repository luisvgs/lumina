import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import math.Fractional.Implicits.infixFractionalOps
import math.Integral.Implicits.infixIntegralOps
import math.Numeric.Implicits.infixNumericOps

import cats.parse.{Parser0, Parser => P}
import cats.parse.Rfc5234.digit

def expr : P[Expr] = number | bool
// def expr: P[Expr] = (expr ~ whitespace ~ P.char('+') ~ whitespace ~ expr_).as(Expr.Binary(Expr.Integer(1), "+", Expr.Integer(2))).orElse(expr_)
def number : P[Expr]= digit.map((c: Char) => Expr.Integer(c.toInt))
def whitespace: P[Unit] = P.charIn(" \t\r\n").void
def whitespaces0 = whitespace.rep0.void
def bool: P[Expr]= P.string("true").as(Expr.Bool(true)).orElse(P.string("false").as(Expr.Bool(false)))
def parser: P[Expr] = expr 


enum Expr:
  case Integer(n: Int)
  case Bool(b: Boolean)
  case Binary(lhs: Expr, op: String, rhs: Expr)

extension (v: Value)
  def +(a: Value): Int = {
    a match {
      case Value.Integer(n) =>
        v match {
          case Value.Integer(m) => n + m
          case _                => ???
        }
      case _ => ???
    }
  }

enum Opt:
  case Plus, Minus, Div

class Interpreter(ast: List[Expr]):
  def interpret(): Value = eval(ast)

  private def eval(exprs: Seq[Expr]): Value =
    var res = Value.Null
    exprs.foreach { expr =>
      res = stmt_eval(expr)
    }
    res

  private def get_op(op: String): Opt = op match {
    case "+" => Opt.Plus
    case "/" => Opt.Div
    case "-" => Opt.Minus
  }

  private def stmt_eval(expr: Expr): Value = expr match {
    case Expr.Integer(x) => Value.Integer(x)
    case Expr.Bool(b)    => Value.Bool(b)
    case Expr.Binary(lhs, op, rhs) => {
      var x: Value = stmt_eval(lhs)
      var y: Value = stmt_eval(lhs)
      get_op(op) match {
        case Opt.Plus  => Value.Integer(x + y)
        case Opt.Minus => Value.Integer(1 - 1)
        case Opt.Div   => Value.Integer(1 - 1)
      }
    }
  }

case class Token(tpe: TokenType, text: String, startPos: Int)

enum Value:
  case Null
  case Bool(b: Boolean)
  case Integer(x: Int)

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
  val int_expr = parser.parse("2")
  val unwrapped = int_expr match {
    case Right(v) => v match {
    case (_, m) => m
  }
  case Left(_) => ???
}
  var ast = List(unwrapped)
  // var ast = List(Expr.Binary(Expr.Integer(10), "+", Expr.Integer(3)))
  val interpreter = new Interpreter(ast).interpret()
  println(interpreter)
}
