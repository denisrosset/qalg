package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait PackMField[M, @sp(Double, Long) A] extends Any with PackMEuclideanRing[M, A] {
  implicit def M: MatInField[M, A]
  implicit def MutM: MatInField[MutM, A]
  implicit def MutV: VecInField[MutV, A]
}
