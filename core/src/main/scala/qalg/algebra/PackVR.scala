package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait PackVR[V, @sp(Double, Long) A] extends Any {
  implicit def V: VecInRing[V, A]
}
