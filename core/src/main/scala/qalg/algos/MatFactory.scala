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

trait MatFactory[M] extends Any {
  def zeros(nRows: Int, nCols: Int): M
  def ones(nRows: Int, nCols: Int): M
  def eye(nRows: Int, nCols: Int): M
  def eye(n: Int): M
}

object MatFactory {
  implicit def fromAlg[M](implicit ev: AlgMVR[M, _, _]): MatFactory[M] = ev.MFactory
}

final class MatFactoryImpl[M, @sp(Double, Long) A](implicit M: MatInRing[M, A]) extends MatFactory[M] {
  implicit def A: Ring[A] = M.A

  def zeros(nRows: Int, nCols: Int): M = M.fill(nRows, nCols)(A.zero)
  def ones(nRows: Int, nCols: Int): M = M.fill(nRows, nCols)(A.one)
  def eye(nRows: Int, nCols: Int): M = M.tabulate(nRows, nCols)(
    (r, c) => if (r == c) A.one else A.zero
  )
  def eye(n: Int): M = eye(n, n)
}
