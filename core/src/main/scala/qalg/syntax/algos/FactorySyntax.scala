package com.faacets
package qalg
package syntax
package algos

import scala.{specialized => sp}

import algebra._
import qalg.algos._

trait FactorySyntax {
  def zeros[M](nRows: Int, nCols: Int)(implicit M: MatFactory[M]): M = M.zeros(nRows, nCols)
  def ones[M](nRows: Int, nCols: Int)(implicit M: MatFactory[M]): M = M.ones(nRows, nCols)
  def eye[M](nRows: Int, nCols: Int)(implicit M: MatFactory[M]): M = M.eye(nRows, nCols)
  def eye[M](n: Int)(implicit M: MatFactory[M]): M = M.eye(n)
  def zeros[V](n: Int)(implicit V: VecFactory[V]): V = V.zeros(n)
  def ones[V](n: Int)(implicit V: VecFactory[V]): V = V.ones(n)
}
