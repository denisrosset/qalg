package com.faacets.qalg
package syntax

import scala.language.higherKinds

import algebra._
import util._

final class LinOps[L, A](lhs: L)(implicit ev: Lin[L, A]) {
  def linearLength: Int = ev.linearLength(lhs)
  def linearApply(k: Int): A = ev.linearApply(lhs, k)
  def sameShape(rhs: L): Boolean = ev.sameShape(lhs, rhs)
}

final class VecOps[V, A](lhs: V)(implicit ev: Vec[V, A]) {
  // Vec
  def length: Int = ev.length(lhs)
  def apply(k: Int): A = ev.apply(lhs, k)
  def toIndexedSeq: IndexedSeq[A] = ev.toIndexedSeq(lhs)
  def view(at: At1): FunV[A] = ev.view(lhs, at)
  def view(at: ::.type): FunV[A] = ev.view(lhs, at)
  // VecBuilder -- we reuse the same wrapper class because of the `apply` overload
  def apply(at: At1)(implicit ev1: VecBuilder[V, A]): V = ev1.apply(lhs, at)
  def apply(at: ::.type)(implicit ev1: VecBuilder[V, A]): V = ev1.apply(lhs, at)
}

// all operations in VecBuilder have been incorporatedin to VecOps

final class VecMutableOps[V, A](lhs: V)(implicit ev: VecMutable[V, A]) {
  def copy: V = ev.copy(lhs)
  def update(k: Int, a: A): Unit = ev.update(lhs, k, a)
  def update(at: At1, a: A): Unit = ev.update(lhs, at, a)
  def update[V1](at: At1, v1: V1)(implicit ev1: Vec[V1, A]): Unit = ev.update[V1](lhs, at, v1)
}

final class MatOps[M, A](lhs: M)(implicit ev: Mat[M, A]) {
  // Mat
  def size: IntInt = ev.size(lhs)
  def nRows: Int = ev.nRows(lhs)
  def nCols: Int = ev.nCols(lhs)
  def apply(r: Int, c: Int): A = ev(lhs, r, c)
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
  def apply(rows: At1, cols: At1)(implicit ev1: MatBuilder[M, A]): M = ev1(lhs, rows, cols)
  def apply(rows: ::.type, cols: ::.type)(implicit ev1: MatBuilder[M, A]): M = ev1(lhs, rows, cols)
  def apply(rows: ::.type, cols: At1)(implicit ev1: MatBuilder[M, A]): M = ev1(lhs, rows, cols)
  def apply(rows: At1, cols: ::.type)(implicit ev1: MatBuilder[M, A]): M = ev1(lhs, rows, cols)
  // MatVecBuilder
  def apply[V](r: Int, cols: At1)(implicit ev1: MatVecBuilder[M, V, A]): V = ev1(lhs, r, cols)
  def apply[V](r: Int, cols: ::.type)(implicit ev1: MatVecBuilder[M, V, A]): V = ev1(lhs, r, cols)
  def apply[V](rows: At1, c: Int)(implicit ev1: MatVecBuilder[M, V, A]): V = ev1(lhs, rows, c)
  def apply[V](rows: ::.type, c: Int)(implicit ev1: MatVecBuilder[M, V, A]): V = ev1(lhs, rows, c)
}

final class MatBuilderOps[M, A](lhs: M)(implicit ev: MatBuilder[M, A]) {
  def t: M = ev.t(lhs)
}

// MatVecBuilder is in MatOps

final class MatInRingOps[M, A](m: M)(implicit ev: MatInRing[M, A]) {
  // MatVecInRing
  def *::[V](lhs: V)(implicit ev1: MatVecInRing[M, V, A]): V = ev1.timesl2(lhs, m)
  def ::*[V](rhs: V)(implicit ev1: MatVecInRing[M, V, A]): V = ev1.timesr2(m, rhs)
}

final class MatMutableOps[M, A](lhs: M)(implicit ev: MatMutable[M, A]) {
  def copy: M = ev.copy(lhs)
  def update(r: Int, c: Int, a: A): Unit = ev.update(lhs, r, c, a)
  def update[V1](rows: At1, c: Int, va1: V1)(implicit ev1: Vec[V1, A]): Unit = ev.update(lhs, rows, c, va1)
  def update[V1](r: Int, cols: At1, va1: V1)(implicit ev1: Vec[V1, A]): Unit = ev.update(lhs, r, cols, va1)
  def update[V1](rows: ::.type, c: Int, va1: V1)(implicit ev1: Vec[V1, A]): Unit = ev.update(lhs, rows, c, va1)
  def update[V1](r: Int, cols: ::.type, va1: V1)(implicit ev1: Vec[V1, A]): Unit = ev.update(lhs, r, cols, va1)
  def update(rows: At1, c: Int, a: A): Unit = ev.update(lhs, rows, c, a)
  def update(r: Int, cols: At1, a: A): Unit = ev.update(lhs, r, cols, a)
  def update(rows: ::.type, c: Int, a: A): Unit = ev.update(lhs, rows, c, a)
  def update(r: Int, cols: ::.type, a: A): Unit = ev.update(lhs, r, cols, a)
  def update[M1](rows: At1, cols: At1, ma1: M1)(implicit ev1: Mat[M1, A]): Unit = ev.update(lhs, rows, cols, ma1)
  def update[M1](rows: ::.type, cols: ::.type, ma1: M1)(implicit ev1: Mat[M1, A]): Unit = ev.update(lhs, rows, cols, ma1)
  def update[M1](rows: ::.type, cols: At1, ma1: M1)(implicit ev1: Mat[M1, A]): Unit = ev.update(lhs, rows, cols, ma1)
  def update[M1](rows: At1, cols: ::.type, ma1: M1)(implicit ev1: Mat[M1, A]): Unit = ev.update(lhs, rows, cols, ma1)
  def update(rows: At1, cols: At1, a: A): Unit = ev.update(lhs, rows, cols, a)
  def update(rows: ::.type, cols: ::.type, a: A): Unit = ev.update(lhs, rows, cols, a)
  def update(rows: ::.type, cols: At1, a: A): Unit = ev.update(lhs, rows, cols, a)
  def update(rows: At1, cols: ::.type, a: A): Unit = ev.update(lhs, rows, cols, a)
}
