package com.faacets.qalg
package syntax

import scala.language.higherKinds

import algebra._
import util._

final class VecOps[V[_], A](lhs: V[A])(implicit ev: Vec[V, A]) {
  def length: Int = ev.length(lhs)
  def apply(k: Int): A = ev.apply(lhs, k)
  def apply(at: At1)(implicit ev1: BuildableVec[V, A]): V[A] = ev1.apply(lhs, at)
}

final class MatOps[M[_], V[_], A](lhs: M[A])(implicit ev: Mat[M, V, A]) {
  def size: IntInt = ev.size(lhs)
  def nRows: Int = ev.nRows(lhs)
  def nCols: Int = ev.nCols(lhs)
  def apply(r: Int, c: Int): A = ev(lhs, r, c)

  def apply(rows: At1, cols: At1)(implicit ev1: BuildableMat[M, V, A]): M[A] = ev1(lhs, rows, cols)
  def apply(rows: ::.type, cols: ::.type)(implicit ev1: BuildableMat[M, V, A]): M[A] = ev1(lhs, rows, cols)

  def apply(rows: ::.type, cols: At1)(implicit ev1: BuildableMat[M, V, A]): M[A] = ev1(lhs, rows, cols)
  def apply(rows: At1, cols: ::.type)(implicit ev1: BuildableMat[M, V, A]): M[A] = ev1(lhs, rows, cols)

  def apply(r: Int, cols: At1)(implicit ev1: BuildableMatVec[M, V, A]): V[A] = ev1(lhs, r, cols)
  def apply(r: Int, cols: ::.type)(implicit ev1: BuildableMatVec[M, V, A]): V[A] = ev1(lhs, r, cols)

  def apply(rows: At1, c: Int)(implicit ev1: BuildableMatVec[M, V, A]): V[A] = ev1(lhs, rows, c)
  def apply(rows: ::.type, c: Int)(implicit ev1: BuildableMatVec[M, V, A]): V[A] = ev1(lhs, rows, c)
}
