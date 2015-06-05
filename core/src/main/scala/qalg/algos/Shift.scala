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

trait Shift[M] {
  def circShift(m: M, rowShift: Int, colShift: Int): M
}

trait ShiftImpl[M, @sp(Double, Long) A] extends Shift[M] {
  implicit def M: MatBuilder[M, A]

  def circShift(m: M, rowShift: Int, colShift: Int): M = {
    val nR = M.nRows(m)
    val nC = M.nCols(m)
    M.tabulate(nR, nC) { (r, c) => M.apply(m, (r + rowShift) % nR, (c + colShift) % nC) }
  }
}
