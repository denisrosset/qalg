package com.faacets
package alg

import scala.util.parsing.combinator._
import spire.math.Rational

object RationalParser extends RationalParserTrait { }

trait RationalParserTrait extends RegexParsers {
  def sign = ("+" ^^ (s => 1)) | ("-" ^^ (s => -1))
  def integer: Parser[Int] = """-?\d+""".r ^^ { _.toInt }
  def posinteger: Parser[Int] = """\d+""".r ^^ { _.toInt }
  def integerAsRational: Parser[Rational] = integer ^^ { Rational(_, 1) }
  def rationalCoefficientOptionalSign: Parser[Rational] = opt(sign) ~ opt(posrational) ^^ {
    case (Some(s) ~ Some(r)) => r * s
    case (None ~ Some(r)) => r
    case (Some(s) ~ None) => Rational(s, 1)
    case (None ~ None) => Rational(1, 1)
  }
  def rationalCoefficientForceSign: Parser[Rational] = sign ~ opt(posrational) ^^ {
    case (s ~ Some(r)) => r * s
    case (s ~ None) => Rational(s, 1)
  }
  def rational: Parser[Rational] = integer ~ opt("/" ~> posinteger) ^^ {
    case (num ~ Some(denom)) => Rational(num, denom)
    case (num ~ None) => Rational(num, 1)
  }
  def posrational: Parser[Rational] = posinteger ~ opt("/" ~> posinteger) ^^ {
    case (num ~ Some(denom)) => Rational(num, denom)
    case (num ~ None) => Rational(num, 1)
  }
}
