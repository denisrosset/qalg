package com.faacets.qalg
package algebra

import scala.{specialized => sp}
import scala.annotation.tailrec

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.field._

import indup.algebra._

trait MatField[M, @sp(Double, Long) A] extends Any with MatRing[M, A] with VectorSpace[M, A] { self =>
  override implicit def scalar: Field[A]

  // override def divr(v: V, a: A): V
}

object MatField {
  def apply[M, @sp(Double, Long) A](implicit M: MatField[M, A]): MatField[M, A] = M
}
