package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait PackVField[V, @sp(Double, Long) A] extends Any with PackVEuclideanRing[V, A] {
  implicit def V: VecInField[V, A]
  implicit def MutM: MatInField[MutM, A]
  implicit def MutV: VecInField[MutV, A]
}
