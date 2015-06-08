package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait PackUM[M, @sp(Double, Long) A] extends Any {
  implicit def UM: MatMutable[M, A]
}
