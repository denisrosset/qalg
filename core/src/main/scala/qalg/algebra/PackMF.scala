package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait PackMF[M, @sp(Double, Long) A] extends Any with PackMR[M, A] {
  implicit def M: MatInField[M, A]
}
