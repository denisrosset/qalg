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

trait MatShift[M] extends Any {
  def circShifted(m: M, rowShift: Int, colShift: Int): M
  def rowsPermuted(m: M, r1: Int, r2: Int): M
  def rowsPermuted(m: M, rowPerm: Array[Int]): M
  def colsPermuted(m: M, c1: Int, c2: Int): M
  def colsPermuted(m: M, colPerm: Array[Int]): M
}

trait MutableMatShift[M] extends Any with MatShift[M] {
  def circShift(m: M, rowShift: Int, colShift: Int): Unit
  def rowsPermute(m: M, r1: Int, r2: Int): Unit // TODO optimize
  def rowsPermuteInverse(m: M, rowInversePerm: Array[Int]): Unit
  def colsPermute(m: M, c1: Int, c2: Int): Unit
  def colsPermuteInverse(m: M, colInversePerm: Array[Int]): Unit
}

object MatShift {
  implicit def fromPack[M](implicit pack: PackRing.ForM[M, _]): MatShift[M] = pack.MShift
}
