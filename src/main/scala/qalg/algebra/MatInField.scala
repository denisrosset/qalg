package com.faacets.qalg
package algebra

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.eq._
import util._

trait MatInField[MA, @sp(Double, Long) A] extends Any with MatInRing[MA, A] with VectorSpace[MA, A] {
  implicit def scalar: Field[A]
}

trait ConvertedMatInField[M, @sp(Double, Long) A, J] extends Any
    with ConvertedMat[M, A, J]
    with MatInField[M, A] {
  def source: MatInField[M, J]

}
