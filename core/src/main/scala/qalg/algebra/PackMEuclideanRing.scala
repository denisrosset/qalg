package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait PackMEuclideanRing[M, @sp(Double, Long) A] extends Any with PackMRing[M, A] {
  implicit def M: MatInEuclideanRing[M, A]
  implicit def MutM: MatInEuclideanRing[MutM, A]
  implicit def MutV: VecInEuclideanRing[MutV, A]
}
