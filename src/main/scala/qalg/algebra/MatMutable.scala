package com.faacets.qalg
package algebra

import scala.language.higherKinds

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.eq._
import util._

trait MatMutable[MA, @sp(Double, Long) A] extends Any with Mat[MA, A] { self =>
  // scalar

  def update(m: MA, r: Int, c: Int, a: A): Unit

  // (sub)vector

  def update[VA1](m: MA, rows: At1, c: Int, va1: VA1)(implicit ev1: Vec[VA1, A]): Unit = {
    val nR = rows.length
    require(nR == ev1.length(va1))
    cforRange(0 until nR) { r =>
      update(m, rows(r), c, ev1.apply(va1, r))
    }
  }
  def update[VA1](m: MA, r: Int, cols: At1, va1: VA1)(implicit ev1: Vec[VA1, A]): Unit = {
    val nC = cols.length
    require(nC == ev1.length(va1))
    cforRange(0 until nC) { c =>
      update(m, r, cols(c), ev1.apply(va1, c))
    }
  }
  def update[VA1](m: MA, rows: ::.type, c: Int, va1: VA1)(implicit ev1: Vec[VA1, A]): Unit =
    update(m, AtRange1(0 until nRows(m)), c, va1)
  def update[VA1](m: MA, r: Int, cols: ::.type, va1: VA1)(implicit ev1: Vec[VA1, A]): Unit =
    update(m, r, AtRange1(0 until nCols(m)), va1)

  def update(m: MA, rows: At1, c: Int, a: A): Unit = {
    val nR = rows.length
    cforRange(0 until nR) { r =>
      update(m, rows(r), c, a)
    }
  }
  def update(m: MA, r: Int, cols: At1, a: A): Unit = {
    val nC = cols.length
    cforRange(0 until nC) { c =>
      update(m, r, cols(c), a)
    }
  }
  def update(m: MA, rows: ::.type, c: Int, a: A): Unit =
    update(m, AtRange1(0 until nRows(m)), c, a)
  def update(m: MA, r: Int, cols: ::.type, a: A): Unit =
    update(m, r, AtRange1(0 until nCols(m)), a)

  // submatrix
  def update[MA1](m: MA, rows: At1, cols: At1, ma1: MA1)(implicit ev1: Mat[MA1, A]): Unit = {
    val nR = rows.length
    val nC = cols.length
    require(nR == ev1.nRows(ma1))
    require(nC == ev1.nCols(ma1))
    cforRange(0 until nR) { r =>
      cforRange(0 until nC) { c =>
        update(m, rows(r), cols(c), ev1.apply(ma1, r, c))
      }
    }
  }
  def update[MA1](m: MA, rows: ::.type, cols: ::.type, ma1: MA1)(implicit ev1: Mat[MA1, A]): Unit =
    update(m, AtRange1(0 until nRows(m)), AtRange1(0 until nCols(m)), ma1)
  def update[MA1](m: MA, rows: ::.type, cols: At1, ma1: MA1)(implicit ev1: Mat[MA1, A]): Unit =
    update(m, AtRange1(0 until nRows(m)), cols, ma1)
  def update[MA1](m: MA, rows: At1, cols: ::.type, ma1: MA1)(implicit ev1: Mat[MA1, A]): Unit =
    update(m, rows, AtRange1(0 until nCols(m)), ma1)

  def update(m: MA, rows: At1, cols: At1, a: A): Unit = {
    val nR = rows.length
    val nC = cols.length
    cforRange(0 until nR) { r =>
      cforRange(0 until nC) { c =>
        update(m, rows(r), cols(c), a)
      }
    }
  }
  def update(m: MA, rows: ::.type, cols: ::.type, a: A): Unit =
    update(m, AtRange1(0 until nRows(m)), AtRange1(0 until nCols(m)), a)
  def update(m: MA, rows: ::.type, cols: At1, a: A): Unit =
    update(m, AtRange1(0 until nRows(m)), cols, a)
  def update(m: MA, rows: At1, cols: ::.type, a: A): Unit =
    update(m, rows, AtRange1(0 until nCols(m)), a)
}

object MatMutable {
  def apply[MA, @sp(Double, Long) A](implicit MM: MatMutable[MA, A]): MatMutable[MA, A] = MM
}
