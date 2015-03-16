package com.faacets.qalg

import scala.language.higherKinds

import scala.{specialized => sp}

import spire.algebra._

import algebra._

package object math {
  implicit class MatTemplate[MA, @sp(Double, Long) A](val lhs: MatBuilder[MA, A]) {
    def zeros(nRows: Int, nCols: Int)(implicit A: AdditiveMonoid[A]): MA =
      lhs.from(FunM.fill(nRows, nCols)(A.zero))
    def ones(nRows: Int, nCols: Int)(implicit A: MultiplicativeMonoid[A]): MA =
      lhs.from(FunM.fill(nRows, nCols)(A.one))
    def eye(nRows: Int, nCols: Int)(implicit A: Ring[A]): MA =
      lhs.from(FunM.fillDiag(nRows, nCols)(A.one, A.zero))
    def eye(n: Int)(implicit A: Ring[A]): MA = eye(n, n)
  }
  implicit class VecTemplate[VA, @sp(Double, Long) A](val lhs: VecBuilder[VA, A]) {
    def zeros(n: Int)(implicit A: AdditiveMonoid[A]): VA =
      lhs.from(FunV.fill(n)(A.zero))
    def ones(n: Int)(implicit A: MultiplicativeMonoid[A]): VA =
      lhs.from(FunV.fill(n)(A.one))
  }
}
