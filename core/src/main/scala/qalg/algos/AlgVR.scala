package com.faacets.qalg
package algos

import scala.{specialized => sp}

import scala.reflect.ClassTag

import algebra._

trait AlgVR[V0, @sp(Double, Long) A] extends Any with PackVR[V0, A] {
  implicit def VFactory: VecFactory[V0]
  implicit def VKron: Kron[V0]
  implicit def VCat: VecCat[V0, A]
  implicit def VShift: VecShift[V0]
}

trait AlgUVR[V0, @sp(Double, Long) A] extends Any with AlgVR[V0, A] with PackUV[V0, A] {
  implicit def VShift: MutableVecShift[V0]
}
