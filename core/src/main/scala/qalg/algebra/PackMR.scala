package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait PackMR[M, @sp(Double, Long) A] extends Any {
  implicit def M: MatInRing[M, A]
}
