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
  implicit class VecShiftOps[V](v: V)(implicit V: VecShift[V]) {
    def circShifted(shift: Int): V = V.circShifted(v, shift)
    def permuted(k1: Int, k2: Int): V = V.permuted(v, k1, k2)
    def permuted(perm: Array[Int]): V = V.permuted(v, perm)
  }
  implicit class VecMutableShiftOps[V](v: V)(implicit V: MutableVecShift[V]) {
    def circShift(shift: Int): Unit = V.circShift(v, shift)
    def permute(k1: Int, k2: Int): Unit = V.permute(v, k1, k2)
    def permuteInverse(permInverse: Array[Int]): Unit = V.permuteInverse(v, permInverse)
  }
  implicit class MatShiftOps[M](m: M)(implicit M: MatShift[M]) {
    def circShifted(rowShift: Int, colShift: Int): M = M.circShifted(m, rowShift, colShift)
    def rowsPermuted(r1: Int, r2: Int): M = M.rowsPermuted(m, r1, r2)
    def rowsPermuted(rowPerm: Array[Int]): M = M.rowsPermuted(m, rowPerm)
    def colsPermuted(c1: Int, c2: Int): M = M.colsPermuted(m, c1, c2)
    def colsPermuted(colPerm: Array[Int]): M = M.colsPermuted(m, colPerm)
  }
  implicit class MatMutableShiftOps[M](m: M)(implicit M: MutableMatShift[M]) {
    def circShift(rowShift: Int, colShift: Int): Unit = M.circShift(m, rowShift, colShift)
    def rowsPermute(r1: Int, r2: Int): Unit = M.rowsPermute(m, r1, r2)
    def colsPermute(c1: Int, c2: Int): Unit = M.colsPermute(m, c1, c2)
    def rowsPermuteInverse(rowPermInverse: Array[Int]): Unit = M.rowsPermuteInverse(m, rowPermInverse)
    def colsPermuteInverse(colPermInverse: Array[Int]): Unit = M.colsPermuteInverse(m, colPermInverse)
  }
  implicit class RrefOps[M](m: M)(implicit M: Rref[M]) {
    def rref: RrefDecomposition[M] = M.rref(m)
  }
  implicit class MutableRrefOps[M](m: M)(implicit M: MutableRref[M]) {
    def unsafeRref: RrefDecomposition[M] = M.unsafeRref(m)
  }
  implicit class LUOps[M, V, @sp(Double) A](m: M)(implicit M: LU[M, V, A]) { // not @sp(Long)
    def lu: LUDecomposition[M, V, A] = M.lu(m)
  }
  implicit class MutableLUOps[M, V, @sp(Double) A](m: M)(implicit M: MutableLU[M, V, A]) { // not @sp(Long)
    def unsafeLU(m: M): LUDecomposition[M, V, A] = M.unsafeLU(m)
  }
  implicit class GramSchmidtOps[M](m: M)(implicit M: GramSchmidt[M]) {
    def gramSchmidt: M = M.gramSchmidt(m)
  }
  implicit class MutableGramSchmidtOps[M](m: M)(implicit M: MutableGramSchmidt[M]) {
    def unsafeGramSchmidt: Unit = M.unsafeGramSchmidt(m)
  }
}
