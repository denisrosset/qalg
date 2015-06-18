package com.faacets.qalg
package algebra

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.eq._
import util._

trait MatMutable[M, @sp(Double, Long) A] extends Any with Update2[M, A] { self =>
  implicit def M: Mat[M, A]

  def copy(m: M): M

  // (sub)vector

  def update[V](m: M, genRows: At1, c: Int, v: V)(implicit V: Vec[V, A]): Unit = {
    val rows = genRows.forRowsOf(m)(M)
    val nR = rows.length
    require(nR == V.length(v))
    cforRange(0 until nR) { r =>
      update(m, rows(r), c, V.apply(v, r))
    }
  }

  def update[V](m: M, r: Int, genCols: At1, v: V)(implicit V: Vec[V, A]): Unit = {
    val cols = genCols.forColsOf(m)(M)
    val nC = cols.length
    require(nC == V.length(v))
    cforRange(0 until nC) { c =>
      update(m, r, cols(c), V.apply(v, c))
    }
  }

  def update(m: M, genRows: At1, c: Int, a: A): Unit = {
    val rows = genRows.forRowsOf(m)(M)
    val nR = rows.length
    cforRange(0 until nR) { r =>
      update(m, rows(r), c, a)
    }
  }

  def update(m: M, r: Int, genCols: At1, a: A): Unit = {
    val cols = genCols.forColsOf(m)(M)
    val nC = cols.length
    cforRange(0 until nC) { c =>
      update(m, r, cols(c), a)
    }
  }

  // submatrix
  def update[M1](m: M, genRows: At1, genCols: At1, m1: M1)(implicit M1: Mat[M1, A]): Unit = {
    val rows = genRows.forRowsOf(m)(M)
    val cols = genCols.forColsOf(m)(M)
    val nR = rows.length
    val nC = cols.length
    require(nR == M1.nRows(m1))
    require(nC == M1.nCols(m1))
    cforRange(0 until nR) { r =>
      cforRange(0 until nC) { c =>
        update(m, rows(r), cols(c), M1.apply(m1, r, c))
      }
    }
  }

  def update(m: M, genRows: At1, genCols: At1, a: A): Unit = {
    val rows = genRows.forRowsOf(m)(M)
    val cols = genCols.forColsOf(m)(M)
    val nR = rows.length
    val nC = cols.length
    cforRange(0 until nR) { r =>
      cforRange(0 until nC) { c =>
        update(m, rows(r), cols(c), a)
      }
    }
  }
}

object MatMutable {
  def apply[M, @sp(Double, Long) A](implicit M: MatMutable[M, A]): MatMutable[M, A] = M
  implicit def fromPack[M, @sp(Double, Long) A](implicit ev: PackUM[M, A]): MatMutable[M, A] = ev.UM
}
