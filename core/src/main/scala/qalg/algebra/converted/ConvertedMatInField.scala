package com.faacets.qalg
package algebra
package converted

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.eq._
import util._

trait ConvertedMatInField[M, @sp(Double, Long) A, J] extends Any
    with ConvertedMatInRing[M, A, J]
    with MatInField[M, A] {
  def source: MatInField[M, J]
}
