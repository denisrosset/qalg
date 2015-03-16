package com.faacets.qalg
package syntax

import scala.language.higherKinds

import algebra._
import util._

final class VecOps[VA, A](lhs: VA)(implicit ev: Vec[VA, A]) {
  def length: Int = ev.length(lhs)
  def apply(k: Int): A = ev.apply(lhs, k)
  def apply(at: At1)(implicit ev1: VecBuilder[VA, A]): VA = ev1.apply(lhs, at)
  def at(k: Int): A = ev.apply(lhs, k)
  def at(at: At1)(implicit ev1: VecBuilder[VA, A]): VA = ev1.apply(lhs, at)
  def rowMat[MA](implicit M: MatBuilder[MA, A]): MA = ev.rowMat[MA](lhs)
  def colMat[MA](implicit M: MatBuilder[MA, A]): MA = ev.colMat[MA](lhs)
}

final class VecMutableOps[VA, A](lhs: VA)(implicit ev: VecMutable[VA, A]) {
  def update(k: Int, a: A): Unit = ev.update(lhs, k, a)
}

final class MatOps[MA, A](lhs: MA)(implicit ev: Mat[MA, A]) {
  def size: IntInt = ev.size(lhs)
  def nRows: Int = ev.nRows(lhs)
  def nCols: Int = ev.nCols(lhs)
  def apply(r: Int, c: Int): A = ev(lhs, r, c)
  def apply(rows: At1, cols: At1)(implicit ev1: MatBuilder[MA, A]): MA = ev1(lhs, rows, cols)
  def apply(rows: ::.type, cols: ::.type)(implicit ev1: MatBuilder[MA, A]): MA = ev1(lhs, rows, cols)
  def apply(rows: ::.type, cols: At1)(implicit ev1: MatBuilder[MA, A]): MA = ev1(lhs, rows, cols)
  def apply(rows: At1, cols: ::.type)(implicit ev1: MatBuilder[MA, A]): MA = ev1(lhs, rows, cols)
  def at(r: Int, c: Int): A = ev(lhs, r, c)
  def at(rows: At1, cols: At1)(implicit ev1: MatBuilder[MA, A]): MA = ev1(lhs, rows, cols)
  def at(rows: ::.type, cols: ::.type)(implicit ev1: MatBuilder[MA, A]): MA = ev1(lhs, rows, cols)
  def at(rows: ::.type, cols: At1)(implicit ev1: MatBuilder[MA, A]): MA = ev1(lhs, rows, cols)
  def at(rows: At1, cols: ::.type)(implicit ev1: MatBuilder[MA, A]): MA = ev1(lhs, rows, cols)
  def vec[VA](r: Int, cols: At1)(implicit ev1: VecBuilder[VA, A]): VA = ev.vec(lhs, r, cols)
  def vec[VA](r: Int, cols: ::.type)(implicit ev1: VecBuilder[VA, A]): VA = ev.vec(lhs, r, cols)
  def vec[VA](rows: At1, c: Int)(implicit ev1: VecBuilder[VA, A]): VA = ev.vec(lhs, rows, c)
  def vec[VA](rows: ::.type, c: Int)(implicit ev1: VecBuilder[VA, A]): VA = ev.vec(lhs, rows, c)
}

final class MatVecOps[MA, A](m: MA)(implicit ev: Mat[MA, A]) {
  def *::[VA](lhs: VA)(implicit ev1: MatVec[MA, VA, A]): VA = ev1.timesl2(lhs, m)
  def ::*[VA](rhs: VA)(implicit ev1: MatVec[MA, VA, A]): VA = ev1.timesr2(m, rhs)
}

final class MatBuilderOps[MA, A](lhs: MA)(implicit ev: MatBuilder[MA, A]) {
  def t: MA = ev.t(lhs)
}

final class MatMutableOps[MA, A](lhs: MA)(implicit ev: MatMutable[MA, A]) {
  def update(r: Int, c: Int, a: A): Unit = ev.update(lhs, r, c, a)
  def updateAt(r: Int, c: Int, a: A): Unit = ev.update(lhs, r, c, a)
}
