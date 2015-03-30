package com.faacets.qalg
package algebra

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.eq._
import util._

trait MatInField[M, @sp(Double, Long) A] extends Any with MatInRing[M, A] with VectorSpace[M, A] {
  implicit def scalar: Field[A]
}

object MatInField {
  def apply[M, @sp(Double, Long) A](implicit M: MatInField[M, A]): MatInField[M, A] = M
}

trait ConvertedMatInField[M, @sp(Double, Long) A, J] extends Any
    with ConvertedMatInRing[M, A, J]
    with MatInField[M, A] {
  def source: MatInField[M, J]

}
