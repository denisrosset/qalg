package com.faacets.qalg
package syntax

import scala.language.higherKinds

import algebra._
import util._

final class VecOps[VA, A](lhs: VA)(implicit ev: Vec[VA, A]) {
  // Vec
  def length: Int = ev.length(lhs)
  def apply(k: Int): A = ev.apply(lhs, k)
  def at(k: Int): A = ev.apply(lhs, k)
  def toIndexedSeq: IndexedSeq[A] = ev.toIndexedSeq(lhs)
  def rowMat[MA](implicit M: MatBuilder[MA, A]): MA = ev.rowMat[MA](lhs)
  def colMat[MA](implicit M: MatBuilder[MA, A]): MA = ev.colMat[MA](lhs)
  // VecBuilder -- we reuse the same wrapper class because of the `apply` overload
  def apply(at: At1)(implicit ev1: VecBuilder[VA, A]): VA = ev1.apply(lhs, at)
  def at(at: At1)(implicit ev1: VecBuilder[VA, A]): VA = ev1.apply(lhs, at)
}

// all operations in VecBuilder have been incorporatedin to VecOps

final class VecMutableOps[VA, A](lhs: VA)(implicit ev: VecMutable[VA, A]) {
  def update(k: Int, a: A): Unit = ev.update(lhs, k, a)
  def update[VA1](at: At1, a: A): Unit = ev.update[VA1](lhs, at, a)
  def update[VA1](at: At1, va1: VA1)(implicit ev1: Vec[VA1, A]): Unit = ev.update[VA1](lhs, at, va1)

  // duplicated methods in case of name collision

  def updateAt(k: Int, a: A): Unit = ev.update(lhs, k, a)
  def updateAt[VA1](at: At1, a: A): Unit = ev.update[VA1](lhs, at, a)
  def updateAt[VA1](at: At1, va1: VA1)(implicit ev1: Vec[VA1, A]): Unit = ev.update[VA1](lhs, at, va1)
}

final class MatOps[MA, A](lhs: MA)(implicit ev: Mat[MA, A]) {
  // Mat
  def size: IntInt = ev.size(lhs)
  def nRows: Int = ev.nRows(lhs)
  def nCols: Int = ev.nCols(lhs)
  def apply(r: Int, c: Int): A = ev(lhs, r, c)
  def at(r: Int, c: Int): A = ev(lhs, r, c)
  def view(rows: At1, cols: At1): FunM[A] = ev.view(lhs, rows, cols)
  def view(rows: ::.type, cols: ::.type): FunM[A] = ev.view(lhs, rows, cols)
  def view(rows: ::.type, cols: At1): FunM[A] = ev.view(lhs, rows, cols)
  def view(rows: At1, cols: ::.type): FunM[A] = ev.view(lhs, rows, cols)
  def view(r: Int, cols: At1): FunV[A] = ev.view(lhs, r, cols)
  def view(r: Int, cols: ::.type): FunV[A] = ev.view(lhs, r, cols)
  def view(rows: At1, c: Int): FunV[A] = ev.view(lhs, rows, c)
  def view(rows: ::.type, c: Int): FunV[A] = ev.view(lhs, rows, c)
  def viewT: FunM[A] = ev.viewT(lhs)
  // Below, we reuse the same wrapper class for other typeclasses,
  // because of the `apply` overload
  // MatBuilder
  def apply(rows: At1, cols: At1)(implicit ev1: MatBuilder[MA, A]): MA = ev1(lhs, rows, cols)
  def apply(rows: ::.type, cols: ::.type)(implicit ev1: MatBuilder[MA, A]): MA = ev1(lhs, rows, cols)
  def apply(rows: ::.type, cols: At1)(implicit ev1: MatBuilder[MA, A]): MA = ev1(lhs, rows, cols)
  def apply(rows: At1, cols: ::.type)(implicit ev1: MatBuilder[MA, A]): MA = ev1(lhs, rows, cols)
  def at(rows: At1, cols: At1)(implicit ev1: MatBuilder[MA, A]): MA = ev1(lhs, rows, cols)
  def at(rows: ::.type, cols: ::.type)(implicit ev1: MatBuilder[MA, A]): MA = ev1(lhs, rows, cols)
  def at(rows: ::.type, cols: At1)(implicit ev1: MatBuilder[MA, A]): MA = ev1(lhs, rows, cols)
  def at(rows: At1, cols: ::.type)(implicit ev1: MatBuilder[MA, A]): MA = ev1(lhs, rows, cols)
  // MatVecSlice
  def apply[VA](r: Int, cols: At1)(implicit ev1: MatVecSlice[MA, VA, A]): VA = ev1(lhs, r, cols)
  def apply[VA](r: Int, cols: ::.type)(implicit ev1: MatVecSlice[MA, VA, A]): VA = ev1(lhs, r, cols)
  def apply[VA](rows: At1, c: Int)(implicit ev1: MatVecSlice[MA, VA, A]): VA = ev1(lhs, rows, c)
  def apply[VA](rows: ::.type, c: Int)(implicit ev1: MatVecSlice[MA, VA, A]): VA = ev1(lhs, rows, c)
  def at[VA](r: Int, cols: At1)(implicit ev1: MatVecSlice[MA, VA, A]): VA = ev1(lhs, r, cols)
  def at[VA](r: Int, cols: ::.type)(implicit ev1: MatVecSlice[MA, VA, A]): VA = ev1(lhs, r, cols)
  def at[VA](rows: At1, c: Int)(implicit ev1: MatVecSlice[MA, VA, A]): VA = ev1(lhs, rows, c)
  def at[VA](rows: ::.type, c: Int)(implicit ev1: MatVecSlice[MA, VA, A]): VA = ev1(lhs, rows, c)
}

final class MatBuilderOps[MA, A](lhs: MA)(implicit ev: MatBuilder[MA, A]) {
  def t: MA = ev.t(lhs)
}

// MatVecSlice is in MatOps

final class MatVecProductOps[MA, A](m: MA)(implicit ev: Mat[MA, A]) {
  def *::[VA](lhs: VA)(implicit ev1: MatVecProduct[MA, VA, A]): VA = ev1.timesl2(lhs, m)
  def ::*[VA](rhs: VA)(implicit ev1: MatVecProduct[MA, VA, A]): VA = ev1.timesr2(m, rhs)
}

final class MatMutableOps[MA, A](lhs: MA)(implicit ev: MatMutable[MA, A]) {
  def update(r: Int, c: Int, a: A): Unit = ev.update(lhs, r, c, a)
//  def updateAt(r: Int, c: Int, a: A): Unit = ev.update(lhs, r, c, a) TODO aliases
}

final class MatAlgOps[MA, A](lhs: MA)(implicit ev: MatAlg[MA, A]) {
  def rank: Int = ev.rank(lhs)
  def rref: MA = ev.rref(lhs)
  def det: A = ev.det(lhs)
  def trace: A = ev.trace(lhs)
  def inverse: MA = ev.inverse(lhs)
}
