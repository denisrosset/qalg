package com.faacets.qalg
package algos
package impl

import scala.{specialized => sp}
import scala.annotation.tailrec

import spire.algebra._
import spire.math._
import spire.syntax.monoid._
import spire.syntax.vectorSpace._
import spire.syntax.cfor._

import algebra._
import syntax.all._
import syntax.indup.all._

final class DenseMatCatImpl[M1, @sp(Double, Long) A](implicit val M1: MatBuild[M1, A]) extends MatCat[M1, A] {

  def vertcat[M2](first: M1, rest: M2*)(implicit M2: Mat[M2, A]): M1 = {
    val ncols = first.nCols
    @tailrec def computeNRows(acc: Int, it: Iterator[M2]): Int =
      if (it.hasNext) {
        val m2 = it.next
        require(m2.nCols == ncols)
        computeNRows(acc + m2.nRows, it)
      } else acc
    val nrows = computeNRows(first.nRows, rest.iterator)
    val b = M1.builder(nrows, ncols, M1.options(first))
    @tailrec def fill(startRow: Int, it: Iterator[M2]): Unit =
      if (it.hasNext) {
        val m2 = it.next
        cforRange(0 until m2.nRows) { r =>
          cforRange(0 until m2.nCols) { c =>
            b.add(startRow + r, c, m2(r, c))
          }
        }
        fill(startRow + m2.nRows, it)
      }
    fill(0, rest.iterator)
    b.result()
  }

  def horzcat[M2](first: M1, rest: M2*)(implicit M2: Mat[M2, A]): M1 = {
    val nrows = first.nRows
    @tailrec def computeNCols(acc: Int, it: Iterator[M2]): Int =
      if (it.hasNext) {
        val m2 = it.next
        require(m2.nRows == nrows)
        computeNCols(acc + m2.nCols, it)
      } else acc
    val ncols = computeNCols(first.nCols, rest.iterator)
    val b = M1.builder(nrows, ncols, M1.options(first))
    @tailrec def fill(startCol: Int, it: Iterator[M2]): Unit =
      if (it.hasNext) {
        val m2 = it.next
        cforRange(0 until m2.nRows) { r =>
          cforRange(0 until m2.nCols) { c =>
            b.add(r, startCol + c, m2(r, c))
          }
        }
        fill(startCol + m2.nCols, it)
      }
    fill(0, rest.iterator)
    b.result()
  }
}
