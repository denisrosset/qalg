package com.faacets
package qalg
package syntax
package algos

import qalg.algos._

trait ShiftSyntax {
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
}
