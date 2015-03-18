package com.faacets.qalg
package algebra

import scala.language.higherKinds

import scala.{specialized => sp}
import spire.algebra._
import spire.syntax.eq._
import spire.syntax.cfor._
import util._

trait VecInField[VA, @sp(Double, Long) A] extends Any with VecInRing[VA, A] with VectorSpace[VA, A] {
  implicit def scalar: Field[A]
}
