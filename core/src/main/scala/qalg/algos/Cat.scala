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

trait Cat {
  def cat[V1, V2, @sp(Double, Long) A](first: V1, rest: V2*)(implicit V1: VecBuilder[V1, A], V2: Vec[V2, A]): V1 = {
    val startIndices = (first.length +: rest.map(_.length)).scanLeft(0)(_ + _)
    val ranges = (startIndices zip startIndices.tail).zipWithIndex map {
      case ((start, nextStart), i) => (start until nextStart) -> i
    }
    val map = RangeMap(ranges: _*)
    V1.tabulate(startIndices.last) { k =>
      val vec = map(k)
      if (vec == 0) first(k) else {
        val vecK = k - startIndices(vec)
        rest(vec - 1)(vecK)
      }
    }
  }

  def vertcat[M1, M2, @sp(Double, Long) A](first: M1, rest: M2*)(implicit M1: MatBuilder[M1, A], M2: Mat[M2, A]): M1 = {
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

  def horzcat[M1, M2, A](first: M1, rest: M2*)(implicit M1: MatBuilder[M1, A], M2: Mat[M2, A]): M1 = {
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

  implicit class VecCat[V, @sp(Double, Long) A](val lhs: VecBuilder[V, A]) {
    def cat[V1](vectors: V1*)(implicit V1: Vec[V1, A]): V = {
      require(vectors.nonEmpty)
      val startIndices = vectors.map(_.length).scanLeft(0)(_ + _)
      val ranges = (startIndices zip startIndices.tail).zipWithIndex map {
        case ((start, nextStart), i) => (start until nextStart) -> i
      }
      val map = RangeMap(ranges: _*)
      lhs.tabulate(startIndices.last) { k =>
        val vec = map(k)
        val vecK = k - startIndices(vec)
        vectors(vec)(vecK)
      }
    }
  }

  implicit class MatCat[M, @sp(Double, Long) A](val lhs: MatBuilder[M, A]) {
    // TODO: optimize
    def vertcat[M1](matrices: M1*)(implicit M1: Mat[M1, A]): M = {
      require(matrices.nonEmpty)
      val ncols = matrices.head.nCols
      require(matrices.forall(_.nCols == ncols))
      val startIndices = matrices.map(_.nRows).scanLeft(0)(_ + _)
      val rowRanges = (startIndices zip startIndices.tail).zipWithIndex map {
        case ((start, nextStart), i) => (start until nextStart) -> i
      }
      val rowMap = RangeMap(rowRanges: _*)
      lhs.tabulate(startIndices.last, ncols) { (r, c) =>
        val mat = rowMap(r)
        val matR = r - startIndices(mat)
        matrices(mat)(matR, c)
      }
    }

    def horzcat[M1](matrices: M1*)(implicit M1: Mat[M1, A]): M = {
      require(matrices.nonEmpty)
      val nrows = matrices.head.nRows
      require(matrices.forall(_.nRows == nrows))
      val startIndices = matrices.map(_.nCols).scanLeft(0)(_ + _)
      val colRanges = (startIndices zip startIndices.tail).zipWithIndex map {
        case ((start, nextStart), i) => (start until nextStart) -> i
      }
      val colMap = RangeMap(colRanges: _*)
      lhs.tabulate(nrows, startIndices.last) { (r, c) =>
        val mat = colMap(c)
        val matC = c - startIndices(mat)
        matrices(mat)(r, matC)
      }
    }
  }
}
