package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait PackUV[V, @sp(Double, Long) A] extends Any {
  implicit def UV: VecMutable[V, A]
}
