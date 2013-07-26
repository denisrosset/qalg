package com.faacets
package polytope

import alg._
import scala.util.parsing.combinator._
import spire.math.Rational

trait CddFileFormat extends PolytopeFileFormat {
  override val whiteSpace = """([ \t]|[*].*\n)+""".r
  def hrepresentation = "H-representation" ~ crlf
  def vrepresentation = "V-representation" ~ crlf

  def begin = "begin" ~ crlf
  def end = "end"

  def integertype = "integer" ^^ { a => 'integer }
  def rationaltype = "rational" ^^ { a => 'rational }
  def numbertype = integertype | rationaltype // no support for real numbers, on purpose

  def linearity = "linearity" ~> posinteger >> (n => repN(n, posinteger) <~ crlf) ^^ {
    case (l) => l
  }

  def dataformat = integer ~ integer ~ numbertype <~ crlf ^^ {
    case (rows~dims~typ) => ((rows,dims,typ))
  }
}

// format described in doc of cddlib-094g
object H1999Format extends CddFileFormat {
  def data = begin ~> dataformat >> ( (a:((Int,Int,Symbol))) => { a match {
    case (rows, dims, 'integer) => repN(rows, repN(dims, integerAsRational) <~ crlf) <~ end ^^
      { case (data) => ((rows, dims, 'integer, data)) }
    case (rows, dims, 'rational) => repN(rows, repN(dims, rational) <~ crlf) <~ end ^^
      { case (data) => ((rows, dims, 'rational, data)) }
  } } )

  def content = hrepresentation ~> opt(linearity) ~ data <~ opt(crlf) ^^ {
    case (None~data) => (List.empty[Int], data)
    case (Some(lin)~data) => (lin, data)
  }

  def writePolytope(P: HRepr): String = {
    val str = new StringBuilder()
    str ++= "H-representation\n"
    if (P.aeq.rows > 0)
      str ++= "linearity " + P.aeq.rows + (0 until P.aeq.rows).map(_+1).mkString(" ")
    str ++= "begin\n"
    str ++= (P.aeq.rows+P.a.rows) + " " + (P.a.cols+1) + " rational\n"
    for (r <- 0 until P.aeq.rows) {
      str ++= P.beq(r).toString
      for (c <- 0 until P.aeq.cols) {
        str += ' '
        str ++= (-P.aeq(r, c)).toString
      }
      str += '\n'
    }
    for (r <- 0 until P.a.rows) {
      str ++= P.b(r).toString
      for (c <- 0 until P.a.cols) {
        str += ' '
        str ++= (-P.a(r, c)).toString
      }
      str += '\n'
    }
    str ++= "end\n"
    str.toString
  }

  def readPolytope(s: String): HRepr = {
    val res = parse(content, s).get
    val eq = res._1.length
    val linearity = res._1.toSet
    val ineq = res._2._1 - eq
    val d: Int = res._2._2 - 1
    val data = res._2._4
    val A = alg.mutable.QMatrix.fill(ineq, if (ineq == 0) 0 else d)(Rational.zero)
    val b = alg.mutable.QVector.fill(ineq)(Rational.zero)
    val Aeq = alg.mutable.QMatrix.fill(eq, if (eq == 0) 0 else d)(Rational.zero)
    val beq = alg.mutable.QVector.fill(eq)(Rational.zero)
    var i = 0
    var ieq = 0
    for ((row, r) <- data.view.zipWithIndex) {
      if (linearity.contains(r+1)) {
        beq(ieq) = (row.head).asInstanceOf[Rational]
        for ((cell, c) <- row.tail.view.zipWithIndex) {
          Aeq(ieq, c) = -cell.asInstanceOf[Rational]
        }
        ieq += 1
      } else {
        b(i) = (row.head).asInstanceOf[Rational]
        for ((cell, c) <- row.tail.view.zipWithIndex) {
          A(i, c) = -cell.asInstanceOf[Rational]
        }
        i += 1
      }
    }
    new HRepr(A.toImmutable, b.toImmutable, Aeq.toImmutable, beq.toImmutable, None)
  }
}

object V1999Format extends CddFileFormat {
  def rayorvertex = ("0" | "1") ^^ {
    case "0" => 'ray
    case "1" => 'vertex
  }
  def data = begin ~> dataformat >> ( (a:((Int,Int,Symbol))) => { a match {
    case (rows, dims, 'integer) => repN(rows, rayorvertex ~ repN(dims-1, integer) <~ crlf) <~ end ^^
      { case (data) => ((rows, dims-1, 'integer, data.map(_ match { case (a~b) => (a,b) }))) }
    case (rows, dims, 'rational) => repN(rows, rayorvertex ~ repN(dims-1, rational) <~ crlf) <~ end ^^
      { case (data) => ((rows, dims-1, 'rational, data.map(_ match { case (a~b) => (a,b) }))) }
  } } )

  def content = (vrepresentation ~> data) <~ opt(crlf) 

  def readPolytope(s: String): VRepr = {
    val res = parse(content, s).get
    val n: Int = res._1
    val d: Int = res._2
    assert(res._3 == 'rational) // TODO: generalize
    val data = res._4
    val nE = data.count(_.asInstanceOf[(Symbol, List[Rational])]._1 == 'vertex)
    val nR = data.count(_.asInstanceOf[(Symbol, List[Rational])]._1 == 'ray)
    val E = alg.mutable.QMatrix.fill(nE, if (nE == 0) 0 else d)(Rational.zero)
    val R = alg.mutable.QMatrix.fill(nR, if (nR == 0) 0 else d)(Rational.zero)
    var kE = 0
    var kR = 0

    for ((rw, r) <- data.view.zipWithIndex) {
      val row = rw.asInstanceOf[(Symbol, List[Rational])]
      val isE = row._1 == 'vertex
      for ((cell, c) <- row._2.view.zipWithIndex) {
        if (isE)
          E(kE, c) = cell
        else
          R(kR, c) = cell
      }
      if (isE)
        kE += 1
      else
        kR += 1
    }
    new VRepr(E.toImmutable, R.toImmutable)
  }

  def writePolytope(P: VRepr) = {
    val str = new StringBuilder()
    str ++= "V-representation\n"
    str ++= "begin\n"
    str ++= (P.e.rows + P.r.rows) + " " + (P.e.cols + 1) + " rational\n"
    for (r <- 0 until P.e.rows) {
      str += '1'
      for (c <- 0 until P.e.cols) {
        str += ' '
        str ++= P.e(r, c).toString
      }
      str += '\n'
    }
    for (r <- 0 until P.r.rows) {
      str += '0'
      for (c <- 0 until P.r.cols) {
        str += ' '
        str ++= P.r(r, c).toString
      }
      str += '\n'
    }
    str ++= "end\n"
  }
}
