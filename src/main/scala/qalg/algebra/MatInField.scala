package com.faacets.qalg
package algebra

import scala.language.higherKinds

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.eq._
import util._

trait MatInField[MA, @sp(Double, Long) A] extends Any with MatInRing[MA, A] with VectorSpace[MA, A] {
  implicit def scalar: Field[A]
}
