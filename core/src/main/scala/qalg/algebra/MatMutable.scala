package com.faacets.qalg
package algebra

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.eq._
import util._

trait MatMutable[M, @sp(Double, Long) A] extends Any { self =>
  implicit def M: Mat[M, A]

  def copy(m: M): M

  // scalar

  def update(m: M, r: Int, c: Int, a: A): Unit

  // (sub)vector

  def update[V](m: M, rows: At1, c: Int, v: V)(implicit V: Vec[V, A]): Unit = {
    val nR = rows.length
    require(nR == V.length(v))
    cforRange(0 until nR) { r =>
      update(m, rows(r), c, V.apply(v, r))
    }
  }

  def update[V](m: M, r: Int, cols: At1, v: V)(implicit V: Vec[V, A]): Unit = {
    val nC = cols.length
    require(nC == V.length(v))
    cforRange(0 until nC) { c =>
      update(m, r, cols(c), V.apply(v, c))
    }
  }

  def update[V](m: M, rows: ::.type, c: Int, v: V)(implicit V: Vec[V, A]): Unit =
    update(m, AtRange1(0 until M.nRows(m)), c, v)

  def update[V](m: M, r: Int, cols: ::.type, v: V)(implicit V: Vec[V, A]): Unit =
    update(m, r, AtRange1(0 until M.nCols(m)), v)

  def update(m: M, rows: At1, c: Int, a: A): Unit = {
    val nR = rows.length
    cforRange(0 until nR) { r =>
      update(m, rows(r), c, a)
    }
  }

  def update(m: M, r: Int, cols: At1, a: A): Unit = {
    val nC = cols.length
    cforRange(0 until nC) { c =>
      update(m, r, cols(c), a)
    }
  }

  def update(m: M, rows: ::.type, c: Int, a: A): Unit =
    update(m, AtRange1(0 until M.nRows(m)), c, a)

  def update(m: M, r: Int, cols: ::.type, a: A): Unit =
    update(m, r, AtRange1(0 until M.nCols(m)), a)

  // submatrix
  def update[M1](m: M, rows: At1, cols: At1, m1: M1)(implicit M1: Mat[M1, A]): Unit = {
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

  def update[M1](m: M, rows: ::.type, cols: ::.type, m1: M1)(implicit M1: Mat[M1, A]): Unit =
    update(m, AtRange1(0 until M.nRows(m)), AtRange1(0 until M.nCols(m)), m1)

  def update[M1](m: M, rows: ::.type, cols: At1, m1: M1)(implicit M1: Mat[M1, A]): Unit =
    update(m, AtRange1(0 until M.nRows(m)), cols, m1)

  def update[M1](m: M, rows: At1, cols: ::.type, m1: M1)(implicit M1: Mat[M1, A]): Unit =
    update(m, rows, AtRange1(0 until M.nCols(m)), m1)

  def update(m: M, rows: At1, cols: At1, a: A): Unit = {
    val nR = rows.length
    val nC = cols.length
    cforRange(0 until nR) { r =>
      cforRange(0 until nC) { c =>
        update(m, rows(r), cols(c), a)
      }
    }
  }
  def update(m: M, rows: ::.type, cols: ::.type, a: A): Unit =
    update(m, AtRange1(0 until M.nRows(m)), AtRange1(0 until M.nCols(m)), a)

  def update(m: M, rows: ::.type, cols: At1, a: A): Unit =
    update(m, AtRange1(0 until M.nRows(m)), cols, a)

  def update(m: M, rows: At1, cols: ::.type, a: A): Unit =
    update(m, rows, AtRange1(0 until M.nCols(m)), a)
}

object MatMutable {
  def apply[M, @sp(Double, Long) A](implicit M: MatMutable[M, A]): MatMutable[M, A] = M
}

trait ConvertedMatMutable[M, @sp(Double, Long) A, J] extends Any
    with Converted[A, J]
    with MatMutable[M, A] {
  def source: MatMutable[M, J]
  def update(m: M, r: Int, c: Int, a: A): Unit = source.update(m, r, c, aToJ(a))
  override def update(m: M, rows: At1, c: Int, a: A): Unit = source.update(m, rows, c, aToJ(a))
  override def update(m: M, r: Int, cols: At1, a: A): Unit = source.update(m, r, cols, aToJ(a))
  override def update(m: M, rows: ::.type, c: Int, a: A): Unit = source.update(m, rows, c, aToJ(a))
  override def update(m: M, r: Int, cols: ::.type, a: A): Unit = source.update(m, r, cols, aToJ(a))
  override def update(m: M, rows: At1, cols: At1, a: A): Unit = source.update(m, rows, cols, aToJ(a))
  override def update(m: M, rows: ::.type, cols: ::.type, a: A): Unit = source.update(m, rows, cols, aToJ(a))
  override def update(m: M, rows: ::.type, cols: At1, a: A): Unit = source.update(m, rows, cols, aToJ(a))
  override def update(m: M, rows: At1, cols: ::.type, a: A): Unit = source.update(m, rows, cols, aToJ(a))
  def copy(m: M): M = source.copy(m)

}
