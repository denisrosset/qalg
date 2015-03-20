package com.faacets.qalg
package algos

import scala.{specialized => sp}

import spire.algebra._
import spire.math._
import spire.syntax.monoid._
import spire.syntax.vectorSpace._
import spire.syntax.cfor._

import algebra._
import syntax.all._

trait Factory {
  implicit class MatTemplate[M, @sp(Double, Long) A](val lhs: MatBuilder[M, A]) {
    def zeros(nRows: Int, nCols: Int)(implicit A: AdditiveMonoid[A]): M =
      lhs.from(FunM.fill(nRows, nCols)(A.zero))
    def ones(nRows: Int, nCols: Int)(implicit A: MultiplicativeMonoid[A]): M =
      lhs.from(FunM.fill(nRows, nCols)(A.one))
    def eye(nRows: Int, nCols: Int)(implicit A: Ring[A]): M =
      lhs.from(FunM.fillDiag(nRows, nCols)(A.one, A.zero))
    def eye(n: Int)(implicit A: Ring[A]): M = eye(n, n)
  }
  implicit class VecTemplate[V, @sp(Double, Long) A](val lhs: VecBuilder[V, A]) {
    def zeros(n: Int)(implicit A: AdditiveMonoid[A]): V =
      lhs.from(FunV.fill(n)(A.zero))
    def ones(n: Int)(implicit A: MultiplicativeMonoid[A]): V =
      lhs.from(FunV.fill(n)(A.one))
  }
}
