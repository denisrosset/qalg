package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait PackMVRing[M, V, @sp(Double, Long) A] extends Any with PackVRing[V, A] with PackMRing[M ,A] {
  implicit def MVProduct: MatVecProduct[M, V]
  implicit def MVSlicer: MatSlicer[M, V]
}
