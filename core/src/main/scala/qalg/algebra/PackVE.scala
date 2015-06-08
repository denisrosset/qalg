package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait PackVE[V, @sp(Double, Long) A] extends Any with PackVR[V, A] {
  implicit def V: VecInEuclideanRing[V, A]
}
