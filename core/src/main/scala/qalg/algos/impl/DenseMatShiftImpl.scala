package com.faacets.qalg
package algos
package impl

import scala.{specialized => sp}

import spire.algebra._
import spire.math._
import spire.syntax.monoid._
import spire.syntax.vectorSpace._
import spire.syntax.cfor._

import algebra._
import syntax.all._

final class DenseMatShiftImpl[M, @sp(Double, Long) A](implicit M: MatBuild[M, A]) extends MatShift[M] {
  def circShifted(m: M, rowShift: Int, colShift: Int): M = {
    val nR = M.nRows(m)
    val nC = M.nCols(m)
    M.tabulate(nR, nC, M.options(m)) { (r, c) => M.apply(m, (r + rowShift) % nR, (c + colShift) % nC) }
  }

  def rowsPermuted(m: M, r1: Int, r2: Int): M =
    M.tabulate(M.nRows(m), M.nCols(m), M.options(m)) { (r, c) =>
      if (r == r1) M.apply(m, r2, c)
      else if (r == r2) M.apply(m, r1, c)
      else M.apply(m, r, c)
    }

  def rowsPermuted(m: M, rowPerm: Array[Int]): M =
    M.tabulate(M.nRows(m), M.nCols(m), M.options(m)) { (r, c) => M.apply(m, rowPerm(r), c) }

  def colsPermuted(m: M, c1: Int, c2: Int): M =
    M.tabulate(M.nRows(m), M.nCols(m), M.options(m)) { (r, c) =>
      if (c == c1) M.apply(m, r, c2)
      else if (c == c2) M.apply(m, r, c1)
      else M.apply(m, r, c)
    }

  def colsPermuted(m: M, colPerm: Array[Int]): M =
    M.tabulate(M.nRows(m), M.nCols(m), M.options(m)) { (r, c) => M.apply(m, r, colPerm(c)) }
}

final class DenseMutableMatShiftImpl[M, @sp(Double, Long) A](implicit M: MatBuild[M, A], MM: MatMut[M, A]) extends MutableMatShift[M] {
  // TODO optimize and remove tabulate
  def circShifted(m: M, rowShift: Int, colShift: Int): M = {
    val nR = M.nRows(m)
    val nC = M.nCols(m)
    M.tabulate(nR, nC, M.options(m)) { (r, c) => M.apply(m, (r + rowShift) % nR, (c + colShift) % nC) }
  }

  def rowsPermuted(m: M, r1: Int, r2: Int): M =
    M.tabulate(M.nRows(m), M.nCols(m), M.options(m)) { (r, c) =>
      if (r == r1) M.apply(m, r2, c)
      else if (r == r2) M.apply(m, r1, c)
      else M.apply(m, r, c)
    }

  def rowsPermuted(m: M, rowPerm: Array[Int]): M =
    M.tabulate(M.nRows(m), M.nCols(m), M.options(m)) { (r, c) => M.apply(m, rowPerm(r), c) }

  def colsPermuted(m: M, c1: Int, c2: Int): M =
    M.tabulate(M.nRows(m), M.nCols(m), M.options(m)) { (r, c) =>
      if (c == c1) M.apply(m, r, c2)
      else if (c == c2) M.apply(m, r, c1)
      else M.apply(m, r, c)
    }

  def colsPermuted(m: M, colPerm: Array[Int]): M =
    M.tabulate(M.nRows(m), M.nCols(m), M.options(m)) { (r, c) => M.apply(m, r, colPerm(c)) }

  def circShift(m: M, rowShift: Int, colShift: Int): Unit = ??? // TODO

  def rowsPermute(m: M, r1: Int, r2: Int): Unit = {
    cforRange(0 until M.nCols(m)) { c =>
      val t = M.apply(m, r1, c)
      MM.update(m, r1, c, M.apply(m, r2, c))
      MM.update(m, r2, c, t)
    }
  }

  def rowsPermuteInverse(m: M, rowPermInverse: Array[Int]): Unit = {
    if (M.nCols(m) == 0) return
    val bs = scala.collection.mutable.BitSet.empty
    cforRange(0 until M.nRows(m)) { i => bs += i }
    while (bs.nonEmpty) {
      val start = bs.head
      bs -= start
      if (start != rowPermInverse(start)) {
        cforRange(0 until M.nCols(m)) { c =>
          var last = start
          var current = rowPermInverse(start)
          val temp = M.apply(m, start, c)
          while (current != start) {
            if (c == 0) bs -= current
            MM.update(m, last, c, M.apply(m, current, c))
            last = current
            current = rowPermInverse(current)
          }
          MM.update(m, last, c, temp)
        }
      }
    }
  }

  def colsPermute(m: M, c1: Int, c2: Int): Unit = {
    cforRange(0 until M.nRows(m)) { r =>
      val t = M.apply(m, r, c1)
      MM.update(m, r, c1, M.apply(m, r, c2))
      MM.update(m, r, c2, t)
    }
  }

  def colsPermuteInverse(m: M, colPermInverse: Array[Int]): Unit = {
    if (M.nRows(m) == 0) return
    val bs = scala.collection.mutable.BitSet.empty
    cforRange(0 until M.nCols(m)) { i => bs += i }
    while (bs.nonEmpty) {
      val start: Int = bs.head
      bs -= start
      if (start != colPermInverse(start)) {
        cforRange(0 until M.nRows(m)) { r =>
          var last: Int = start
          var current: Int = colPermInverse(start)
          val temp = M.apply(m, r, start)
          while (current != start) {
            if (r == 0) bs -= current
            MM.update(m, r, last, M.apply(m, r, current))
            last = current
            current = colPermInverse(current)
          }
          MM.update(m, r, last, temp)
        }
      }
    }
  }
}
