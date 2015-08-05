package com.faacets.qalg

import scala.{specialized => sp}

import algebra._
import algos._

trait LowerPriority {
  implicit def matVecProductFromPack[M, V, @sp(Double, Long) A](implicit pack: PackRing.ForMV[M, V, A]): MatVecProduct[M, V] = pack.MatVecProduct
  implicit def matRingSliceFromPack[M, V, @sp(Double, Long) A](implicit pack: PackRing.ForMV[M, V, A]): MatRing[M, A] with MatSlice[M, V, A] = pack.M
  implicit def vecRingFromPack[V, @sp(Double, Long) A](implicit pack: PackRing.ForV[V, A]): VecRing[V, A] = pack.V
}

package object algos extends LowerPriority {
  implicit def matFieldFromPack[M, V, @sp(Double, Long) A](implicit pack: PackField.ForMV[M, V, A]): MatField[M, A] with MatSlice[M, V, A] = pack.M
  implicit def vecFieldFromPack[V, @sp(Double, Long) A](implicit pack: PackField.ForV[V, A]): VecField[V, A] = pack.V
}
