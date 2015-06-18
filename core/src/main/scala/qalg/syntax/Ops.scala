package com.faacets.qalg
package syntax

import scala.language.higherKinds

import algebra._
import util._

final class Index1Ops[T, A](lhs: T)(implicit ev: Index1[T, A]) {
  def apply(t: T, i: Int): A = ev.apply(t, i)
}

final class Index2Ops[T, A](lhs: T)(implicit ev: Index2[T, A]) {
  def apply(t: T, i: Int, j: Int): A = ev.apply(t, i, j)
}

final class Update1Ops[T, A](lhs: T)(implicit ev: Update1[T, A]) {
  def update(t: T, i: Int, a: A): Unit = ev.update(t, i, a)
}

final class Update2Ops[T, A](lhs: T)(implicit ev: Update2[T, A]) {
  def update(t: T, i: Int, j: Int, a: A): Unit = ev.update(t, i, j, a)
}

final class LinOps[L, A](lhs: L)(implicit ev: Lin[L, A]) {
  def linearLength: Int = ev.linearLength(lhs)
  def linearApply(k: Int): A = ev.linearApply(lhs, k)
  def sameShape(rhs: L): Boolean = ev.sameShape(lhs, rhs)
}

final class VecOps[V, A](lhs: V)(implicit ev: Vec[V, A]) {
  // Vec
  def length: Int = ev.length(lhs)
  def toIndexedSeq: IndexedSeq[A] = ev.toIndexedSeq(lhs)
  def view(at: At1): FunV[A] = ev.view(lhs, at)
  // VecBuilder -- we reuse the same wrapper class because of the `apply` overload
  def apply(at: At1)(implicit ev1: VecBuilder[V, A]): V = ev1.apply(lhs, at)
}

// all operations in VecBuilder have been incorporatedin to VecOps

final class VecMutableOps[V, A](lhs: V)(implicit ev: VecMutable[V, A]) {
  def copy: V = ev.copy(lhs)
  def update(at: At1, a: A): Unit = ev.update(lhs, at, a)
  def update[V1](at: At1, v1: V1)(implicit ev1: Vec[V1, A]): Unit = ev.update[V1](lhs, at, v1)
}

final class MatOps[M, A](lhs: M)(implicit ev: Mat[M, A]) {
  // Mat
  def size: IntInt = ev.size(lhs)
  def nRows: Int = ev.nRows(lhs)
  def nCols: Int = ev.nCols(lhs)
  def view(rows: At1, cols: At1): FunM[A] = ev.view(lhs, rows, cols)
  def view(r: Int, cols: At1): FunV[A] = ev.view(lhs, r, cols)
  def view(rows: At1, c: Int): FunV[A] = ev.view(lhs, rows, c)
  def viewT: FunM[A] = ev.viewT(lhs)
  // Below, we reuse the same wrapper class for other typeclasses,
  // because of the `apply` overload
  // MatBuilder
  def apply(rows: At1, cols: At1)(implicit ev1: MatBuilder[M, A]): M = ev1(lhs, rows, cols)
  // MatVecBuilder
  def apply[V](r: Int, cols: At1)(implicit ev1: MatSlicer[M, V]): V = ev1(lhs, r, cols)
  def apply[V](rows: At1, c: Int)(implicit ev1: MatSlicer[M, V]): V = ev1(lhs, rows, c)
}

final class MatBuilderOps[M, A](lhs: M)(implicit ev: MatBuilder[M, A]) {
  def t: M = ev.t(lhs)
}

// MatVecBuilder is in MatOps

final class MatInRingOps[M, A](m: M)(implicit ev: MatInRing[M, A]) {
  // MatVecInRing
  def *::[V](lhs: V)(implicit ev1: MatVecProduct[M, V]): V = ev1.timesl2(lhs, m)
  def ::*[V](rhs: V)(implicit ev1: MatVecProduct[M, V]): V = ev1.timesr2(m, rhs)
}

final class LinBuilderOps[LA, A](l: LA)(implicit ev: LinBuilder[LA, A]) {
  def map(l: LA)(f: A => A): LA = ev.map(l)(f)
}

final class MatMutableOps[M, A](lhs: M)(implicit ev: MatMutable[M, A]) {
  def copy: M = ev.copy(lhs)
  def update[V1](rows: At1, c: Int, va1: V1)(implicit ev1: Vec[V1, A]): Unit = ev.update(lhs, rows, c, va1)
  def update[V1](r: Int, cols: At1, va1: V1)(implicit ev1: Vec[V1, A]): Unit = ev.update(lhs, r, cols, va1)
  def update(rows: At1, c: Int, a: A): Unit = ev.update(lhs, rows, c, a)
  def update(r: Int, cols: At1, a: A): Unit = ev.update(lhs, r, cols, a)
  def update[M1](rows: At1, cols: At1, ma1: M1)(implicit ev1: Mat[M1, A]): Unit = ev.update(lhs, rows, cols, ma1)
  def update(rows: At1, cols: At1, a: A): Unit = ev.update(lhs, rows, cols, a)
}
