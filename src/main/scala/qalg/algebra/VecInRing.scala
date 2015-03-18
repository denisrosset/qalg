package com.faacets.qalg
package algebra

import scala.language.higherKinds

import scala.{specialized => sp}
import spire.algebra._
import spire.syntax.eq._
import spire.syntax.cfor._
import util._

trait VecInRing[VA, @sp(Double, Long) A] extends Any with VecBuilder[VA, A] with Module[VA, A] {
  implicit def scalar: Ring[A]
  def zero: VA = from(FunV.empty[A])
  def kron(x: VA, y: VA): VA = {
    /*
     val res = mutable.QVector.zeros(a.length * b.length)
     // TODO cfor
     for (i <- 0 until a.length; av = a(i)) {
     val prod: V = ev.timesl(av, b)
     res(i * b.length until (i+1) * b.length) = prod
     }
     factory.unsafeBuild(res)
     */
    ???
  }
}
