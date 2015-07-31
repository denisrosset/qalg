package com.faacets.qalg
package algos

import scala.{specialized => sp}

import algebra._
import syntax.all._

trait MatFactory[M] extends Any {
  implicit val M: MatRing[M, _]
  def zeros(nRows: Int, nCols: Int, options: M.Options = M.defaultOptions): M
  def ones(nRows: Int, nCols: Int, options: M.Options = M.defaultOptions): M
  def eye(nRows: Int, nCols: Int, options: M.Options): M
  def eye(n: Int, options: M.Options): M
  def eye(nRows: Int, nCols: Int): M
  def eye(n: Int): M
}

object MatFactory {
  implicit def fromPack[M](implicit ev: PackRing.ForM[M, _]): MatFactory[M] = ev.MFactory
}
