package com.faacets.qalg
package algos

import scala.{specialized => sp}

import spire.algebra._
import spire.math._
import spire.syntax.monoid._
import spire.syntax.vectorSpace._
import spire.syntax.cfor._

import algebra._
import syntax.all._
import util._

trait MatCat[M1, @sp(Double, Long) A] extends Any {
  def vertcat[M2](first: M1, rest: M2*)(implicit M2: Mat[M2, A]): M1
  def horzcat[M2](first: M1, rest: M2*)(implicit M2: Mat[M2, A]): M1
}

object MatCat {
  implicit def fromAlg[M, @sp(Double, Long) A](implicit ev: AlgMVR[M, _, A]): MatCat[M, A] = ev.MCat
}

final class MatCatImpl[M1, @sp(Double, Long) A](implicit M1: MatBuilder[M1, A]) extends MatCat[M1, A] {

  def vertcat[M2](first: M1, rest: M2*)(implicit M2: Mat[M2, A]): M1 = {
    val ncols = first.nCols
    require(rest.forall(_.nCols == ncols))
    val startIndices = (first.nRows +: rest.map(_.nRows)).scanLeft(0)(_ + _)
    val rowRanges = (startIndices zip startIndices.tail).zipWithIndex map {
      case ((start, nextStart), i) => (start until nextStart) -> i
    }
    val rowMap = RangeMap(rowRanges: _*)
    M1.tabulate(startIndices.last, ncols) { (r, c) =>
      val mat = rowMap(r)
      if (mat == 0) first(r, c) else {
        val matR = r - startIndices(mat)
        rest(mat - 1)(matR, c)
      }
    }
  }

  def horzcat[M2](first: M1, rest: M2*)(implicit M2: Mat[M2, A]): M1 = {
    val nrows = first.nRows
    require(rest.forall(_.nRows == nrows))
    val startIndices = (first.nCols +: rest.map(_.nCols)).scanLeft(0)(_ + _)
    val colRanges = (startIndices zip startIndices.tail).zipWithIndex map {
      case ((start, nextStart), i) => (start until nextStart) -> i
    }
    val colMap = RangeMap(colRanges: _*)
    M1.tabulate(nrows, startIndices.last) { (r, c) =>
      val mat = colMap(c)
      if (mat == 0) first(r, c) else {
        val matC = c - startIndices(mat)
        rest(mat - 1)(r, matC)
      }
    }
  }
}
