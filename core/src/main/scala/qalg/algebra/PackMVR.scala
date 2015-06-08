package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait PackMVR[M, V, @sp(Double, Long) A] extends Any with PackVR[V, A] with PackMR[M, A] {
  implicit def MVProduct: MatVecProduct[M, V]
  implicit def MVSlicer: MatSlicer[M, V]
}
