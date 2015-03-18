package com.faacets.qalg
package algebra

import scala.language.higherKinds

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.eq._
import util._

trait MatInRing[MA, @sp(Double, Long) A] extends Any with MatBuilder[MA, A] with Module[MA, A] with MultiplicativeSemigroup[MA] {
  implicit def scalar: Ring[A]
  def zero: MA = from(FunM.empty[A])
  def kron(x: MA, y: MA): MA = {
    /*
     def op(a: M, b: M): M = {
     val res = mutable.QMatrix.zeros(a.rows * b.rows, a.cols * b.cols)
     // TODO cfor
     for (r <- 0 until a.rows; c <- 0 until a.cols; av = a(r, c))
     res((r * b.rows) until ((r+1) * b.rows), (c * b.cols) until ((c+1) * b.cols)) = ms.timesl(av, b)
     a.factory.unsafeBuild(res)
     }
     */
    ???
  }
}
