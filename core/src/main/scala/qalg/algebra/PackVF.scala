package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait PackVF[V, @sp(Double, Long) A] extends Any with PackVE[V, A] {
  implicit def V: VecInField[V, A]
}
