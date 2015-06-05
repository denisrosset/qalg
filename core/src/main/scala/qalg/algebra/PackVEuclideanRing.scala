package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait PackVEuclideanRing[V, @sp(Double, Long) A] extends Any with PackVRing[V, A] {
  implicit def V: VecInEuclideanRing[V, A]
  implicit def MutM: MatInEuclideanRing[MutM, A]
  implicit def MutV: VecInEuclideanRing[MutV, A]
}
