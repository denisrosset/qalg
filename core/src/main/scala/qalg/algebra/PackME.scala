package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait PackME[M, @sp(Double, Long) A] extends Any with PackMR[M, A] {
  implicit def M: MatInEuclideanRing[M, A]
}
