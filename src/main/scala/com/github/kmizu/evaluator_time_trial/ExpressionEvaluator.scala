package com.github.kmizu.evaluator_time_trial
import java.io.File
import scala.io.Source

object ExpressionEvaluator extends RegexParsers {

  def expression: Parser[Int] = chainl1(multitive, ("+"|"-") ^^ { case op =>
    (l: Int, r: Int) => if(op == "+") l + r else if (op == "-") l - r else sys.error(op + " cannot be used")
  })
  
  def multitive: Parser[Int] = chainl1(primary, ("*"|"/") ^^ { case op =>
    (l: Int, r: Int) => if(op == "*") l * r else if (op == "/") l / r else sys.error(op + " cannot be used")
  })

  def primary: Parser[Int] =  ("(" ~> expression <~ ")") | number

  def number: Parser[Int] = regex("""0|[1-9][0-9]*""".r) ^^ (_.toInt)

  def main(args: Array[String]) {
    val src = Source.fromFile(args(0))
    parseAll(expression, src.mkString)
  }
}
