package com.faacets.qalg
package algos
package impl

import scala.{specialized => sp}

import spire.algebra._
import spire.math._
import spire.syntax.monoid._
import spire.syntax.vectorSpace._
import spire.syntax.cfor._

import algebra._
import syntax.all._

final class MatFactoryImpl[M, @sp(Double, Long) A](implicit val M: MatRing[M, A]) extends MatFactory[M] {
  import M.scalar


  def zeros(nRows: Int, nCols: Int, options: M.Options = M.defaultOptions): M = M.fill(nRows, nCols, options)(scalar.zero)
  def ones(nRows: Int, nCols: Int, options: M.Options = M.defaultOptions): M = M.fill(nRows, nCols, options)(scalar.one)


  def eye(n: Int, options: M.Options): M = eye(n, n, options)

  def eye(nRows: Int, nCols: Int, options: M.Options): M = M.tabulate(nRows, nCols)(
    (r, c) => if (r == c) scalar.one else scalar.zero
  )

  def eye(nRows: Int, nCols: Int): M = eye(nRows, nCols, M.defaultOptions)
  def eye(n: Int): M = eye(n, M.defaultOptions)
}
