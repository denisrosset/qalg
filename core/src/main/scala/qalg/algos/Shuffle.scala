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

trait Shuffle {
  def circShift[MA, @sp(Double, Long) A](ma: MA, rowShift: Int, colShift: Int)(implicit MA: MatMutable[MA, A] with MatBuilder[MA, A]) = {
    val res = MA.fromFunM(ma.view(::, ::))
    val rows = ma.nRows
    val cols = ma.nCols
    rowShift.signum match {
      case 1 => { // shift downwards
        val s = rowShift % rows
        if (s != 0) {
          res(0 until s, ::) = ma(rows - s until rows, ::)
          res(s until rows, ::) = ma(0 until rows - s, ::)
        }
      }
      case -1 => { // shift upwards
        val s = (-rowShift) % rows
        if (s != 0) {
          res(rows - s until rows, ::) = ma(0 until s, ::)
          res(0 until rows - s, ::) = ma(s until rows, ::)
        }
      }
      case _ => { }
    }
    colShift.signum match {
      case 1 => { // shift to the right
        val s = colShift % cols
        if (s != 0) {
          res(::, 0 until s) = ma(::, rows - s until rows)
          res(::, s until rows) = ma(::, 0 until rows - s)
        }
      }
      case -1 => { // shift to the left
        val s = (-colShift) % cols
        if (s != 0) {
          res(::, rows - s until rows) = ma(::, 0 until s)
          res(::, 0 until rows - s) = ma(::, s until rows)
        }
      }
      case _ => { }
    }
    res
  }
}
