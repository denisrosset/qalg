package com.faacets.qalg

import scala.{specialized => sp}

import algebra._

package object algos {
  def zeros[M](nRows: Int, nCols: Int)(implicit M: MatFactory[M]): M = M.zeros(nRows, nCols)
  def ones[M](nRows: Int, nCols: Int)(implicit M: MatFactory[M]): M = M.ones(nRows, nCols)
  def eye[M](nRows: Int, nCols: Int)(implicit M: MatFactory[M]): M = M.eye(nRows, nCols)
  def eye[M](n: Int)(implicit M: MatFactory[M]): M = M.eye(n)
  def zeros[V](n: Int)(implicit V: VecFactory[V]): V = V.zeros(n)
  def ones[V](n: Int)(implicit V: VecFactory[V]): V = V.ones(n)
  def kron[L](lins: L*)(implicit L: Kron[L]): L =
    lins.tail.foldLeft(lins.head)(L.kron(_, _))
  def reverseKron[L](lins: L*)(implicit L: Kron[L]): L =
    kron[L](lins.reverse:_*)
  def cat[V1, V2, @sp(Double, Long) A](first: V1, rest: V2*)(implicit V1: VecCat[V1, A], V2: Vec[V2, A]): V1 = V1.cat(first, rest: _*)
  def vertcat[M1, M2, @sp(Double, Long) A](first: M1, rest: M2*)(implicit M1: MatCat[M1, A], M2: Mat[M2, A]): M1 = M1.vertcat(first, rest: _*)
  def horzcat[M1, M2, @sp(Double, Long) A](first: M1, rest: M2*)(implicit M1: MatCat[M1, A], M2: Mat[M2, A]): M1 = M1.horzcat(first, rest: _*)
  implicit class ShiftOps[M](m: M)(implicit M: Shift[M]) {
    def circShift(m: M, rowShift: Int, colShift: Int): M = M.circShift(m, rowShift, colShift)
  }
  implicit class RrefOps[M](m: M)(implicit M: Rref[M]) {
    def rref: RrefDecomposition[M] = M.rref(m)
  }
  implicit class MutableRrefOps[M](m: M)(implicit M: MutableRref[M]) {
    def unsafeRref: RrefDecomposition[M] = M.unsafeRref(m)
  }
  implicit class LUOps[M, V](m: M)(implicit M: LU[M, V]) {
    def lu: LUDecomposition[M, V] = M.lu(m)
  }
  implicit class MutableLUOps[M, V](m: M)(implicit M: MutableLU[M, V]) {
    def unsafeLU(m: M): LUDecomposition[M, V] = M.unsafeLU(m)
  }
  implicit class GramSchmidtOps[M](m: M)(implicit M: GramSchmidt[M]) {
    def gramSchmidt: M = M.gramSchmidt(m)
  }
  implicit class MutableGramSchmidtOps[M](m: M)(implicit M: MutableGramSchmidt[M]) {
    def unsafeGramSchmidt: Unit = M.unsafeGramSchmidt(m)
  }
}
