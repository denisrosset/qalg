package com.faacets
package polytope

import alg._
import scala.util.parsing.combinator._
import spire.math.Rational
import net.alasc.indexSyntax._
import alg.all._

trait PortaFileFormat extends PolytopeFileFormat {
  override val whiteSpace = """([ \t])+""".r
  def dimension = ("DIM" ~ "=") ~> posinteger <~ crlf
  def point(d: Int) = (opt("(" ~ integer ~ ")") ~> repN(d, rational)) <~ crlf

  trait IEQSection { }
  case class EliminationOrder(x: List[Int]) extends IEQSection { }

  def elimination_order(d: Int) = ("ELIMINATION_ORDER" ~ crlf) ~> repN(d, integer) <~ crlf ^^ { x => EliminationOrder(x) }

  def end = "END" ~ opt(crlf)
}

object POIFormat extends PortaFileFormat {
  def conv_section(d: Int) = ("CONV_SECTION" ~ crlf) ~> rep(point(d)) ^^ ( _.map(('vertex, _)) )
  def cone_section(d: Int) = ("CONE_SECTION" ~ crlf) ~> rep(point(d)) ^^ ( _.map(('ray, _)) )
  def poi_element(d: Int) = conv_section(d) | cone_section(d)
  def content = dimension >> ( (d: Int) => rep(poi_element(d)) <~ end ^^ {
    case list => (d,
      list.flatten.groupBy(_._1). // group by either 'vertex or 'ray
        mapValues(_.map(_._2)) // now that elements are grouped in a map by the 'vertex or 'ray key
                               // the symbol 'vertex/'ray associated with each point is no longer needed
    )
  }
  )
  def writePolytope(p: VRepr): String = {
    val str = new StringBuilder()
    str ++= "DIM = " + p.dim + "\n"
    def printMatrix(m: alg.immutable.QMatrix) = 
      (0 until m.rows).map( r =>
        "(" + (r+1) + ") " + (0 until p.e.cols).map(p.e(r,_)).mkString(" ")
      ).mkString("\n")
    if (p.e.rows > 0) {
      str ++= "CONV_SECTION\n"
      str ++= printMatrix(p.e)
      str ++= "\n"
    }
    if (p.r.rows > 0) {
      str ++= "CONE_SECTION\n"
      str ++= printMatrix(p.r)
      str ++= "\n"
    }
    str ++= "END\n"
    str.toString
  }

  def readPolytope(s: String) = {
    val res = parse(content, s).get
    val d: Int = res._1
    val e = res._2.getOrElse('vertex, List.empty[List[Rational]])
    val r = res._2.getOrElse('ray, List.empty[List[Rational]])
    val E = alg.mutable.QMatrix.fill(e.size, if (e.size == 0) 0 else d)(Rational.zero)
    val R = alg.mutable.QMatrix.fill(r.size, if (r.size == 0) 0 else d)(Rational.zero)
    for ((row, r) <- e.view.zipWithIndex) {
      for ((cell, c) <- row.view.zipWithIndex) {
        E(r, c) = cell.asInstanceOf[Rational]
      }
    }
    for ((row, r) <- r.view.zipWithIndex) {
      for ((cell, c) <- row.view.zipWithIndex) {
        R(r, c) = cell.asInstanceOf[Rational]
      }
    }
    new VRepr(E.toImmutable, R.toImmutable)
  }
}

case class Term(c: Rational, k: Int) {
  def unary_- : Term = Term(-c, k)
}

object IEQFormat extends PortaFileFormat {
  case class Valid(x: List[Rational]) extends IEQSection { }
  case class LowerBounds(lb: List[Rational]) extends IEQSection { }
  case class UpperBounds(ub: List[Rational]) extends IEQSection { }
  case class Inequalities(ieq: List[(Symbol, List[Term])]) extends IEQSection { }

  def variable = opt("x" ~> posinteger) ^^ {
    case Some(s) => s
    case None => 0
  }
  def first_term = rationalCoefficientOptionalSign ~ variable ^^ {
    case (coeff ~ v) => Term(coeff, v)
  }
  def term = rationalCoefficientForceSign ~ variable ^^ {
    case (coeff ~ v) => Term(coeff, v)
  }
  def numbering = opt("(" ~ integer ~ ")") 
  def in_equality: Parser[(Symbol, List[Term])] = (numbering ~> (
    first_term ~ rep(term) ~ ("==" | "<=" | ">=") ~ first_term ~ rep(term) <~ crlf)) ^^ {
    case (lf ~ ll ~ "==" ~ rf ~ rl) =>
      ('eq, List(List(lf), ll, List(-rf), rl.map(-_)).flatten)
    case (lf ~ ll ~ "<=" ~ rf ~ rl) => 
      ('ineq, List(List(lf), ll, List(-rf), rl.map(-_)).flatten)
    case (lf ~ ll ~ ">=" ~ rf ~ rl) => 
      ('ineq, List(List(-lf), ll.map(-_), List(rf), rl).flatten)
  }

  def valid(d: Int): Parser[Valid] = ("VALID" ~ crlf) ~> point(d) ^^ { x => Valid(x) }
  def lower_bounds(d: Int): Parser[LowerBounds] = ("LOWER_BOUNDS" ~ crlf) ~> point(d) ^^ { x => LowerBounds(x) }
  def upper_bounds(d: Int): Parser[UpperBounds] = ("UPPER_BOUNDS" ~ crlf) ~> point(d) ^^ { x => UpperBounds(x) }
  def inequalities_section: Parser[Inequalities] = ("INEQUALITIES_SECTION" ~ crlf) ~> rep(in_equality) ^^ { ineqs => Inequalities(ineqs) }

  case class IEQData(d: Int,
    v: Option[alg.mutable.QVector] = None,
    lb: Option[LowerBounds] = None,
    ub: Option[UpperBounds] = None,
    eo: Option[EliminationOrder] = None,
    ineqs: List[Inequalities] = Nil) { }

  def ieq_element(d: Int): Parser[IEQSection] = (
    valid(d)
      | inequalities_section
      | lower_bounds(d)
      | upper_bounds(d)
      | elimination_order(d)
  )
  def content = dimension >> ( (d: Int) => (rep(ieq_element(d)) <~ end) ^^ {
    list => list.foldLeft(IEQData(d)) {
      case (dt, x: Valid) => dt.copy(v = Some(alg.mutable.QVector(x.x:_*)))
      case (dt, x: LowerBounds) => dt.copy(lb = Some(x))
      case (dt, x: UpperBounds) => dt.copy(ub = Some(x))
      case (dt, x: EliminationOrder) => dt.copy(eo = Some(x))
      case (dt, x: Inequalities) => dt.copy(ineqs = x :: dt.ineqs)
    }
  }
  )
  def writePolytope(p: HRepr) = {
    val str = new StringBuilder()
    str ++= "DIM = " + p.dim + "\n"
    def printLinear(a: alg.immutable.QMatrix, b: alg.immutable.QVector, op: String) = {
      assert(a.rows == b.length)
      (0 until a.rows).map ( r => {
        "(" + (r+1) + ") " + (0 until a.cols).filter(a(r,_) != 0).
          map(c => (if (a(r,c) >= 0) "+" else "") + a(r,c) + "x" + (c+1)).mkString + " " + op + " " + b(r).toString
      } ).mkString("\n")
    }
    p.valid match {
      case Some(v) =>
        str ++= "VALID\n"
        import net.alasc._
        str ++= v.indexToIndexedSeq.mkString(" ") + "\n"
      case None =>
    }
    str ++= "INEQUALITIES_SECTION\n"
    str ++= printLinear(p.a, p.b, "<=") + "\n"
    str ++= printLinear(p.aeq, p.beq, "==") + "\n"
    str ++= "END\n"
    str
  }
  def readPolytope(s: String) = {
    val res = parse(content, s).get
    val d: Int = res.d
    val valid = res.v
    val eqineq = res.ineqs.map(_.ieq).flatten.groupBy(_._1).
      mapValues(_.map(_._2))
    val eqs = eqineq.getOrElse('eq, List.empty[List[Term]])
    val ineqs = eqineq.getOrElse('ineq, List.empty[List[Term]])
    val a = alg.mutable.QMatrix.fill(ineqs.size, if (ineqs.size == 0) 0 else d)(Rational.zero)
    val b = alg.mutable.QVector.fill(ineqs.size)(Rational.zero)
    val aeq = alg.mutable.QMatrix.fill(eqs.size, if (eqs.size == 0) 0 else d)(Rational.zero)
    val beq = alg.mutable.QVector.fill(eqs.size)(Rational.zero)
    for ((row, r) <- ineqs.view.zipWithIndex) {
      for ((cell, c) <- row.view.zipWithIndex) {
        if (cell.k == 0)
          b(r) = -cell.c
        else
          a(r, cell.k - 1) = cell.c
      }
    }
    for ((row, r) <- eqs.view.zipWithIndex) {
      for ((cell, c) <- row.view.zipWithIndex) {
        if (cell.k == 0)
          beq(r) = -cell.c
        else
          aeq(r, cell.k - 1) = cell.c
      }
    }
    new HRepr(a.toImmutable, b.toImmutable, aeq.toImmutable, beq.toImmutable, valid.map(_.toImmutable))
  }
}
