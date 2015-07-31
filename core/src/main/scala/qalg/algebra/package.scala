package com.faacets.qalg

import scala.{specialized => sp}

import indup.algebra._

package object algebra {
  type VecMut[V, A] = Update1[V, A]

  object VecMut {
    def apply[V, @sp(Double, Long) A](V: VecMut[V, A]): VecMut[V, A] = V
  }

  type MatMut[M, A] = Update2[M, A]

  object MatMut {
    def apply[M, @sp(Double, Long) A](M: MatMut[M, A]): MatMut[M, A] = M
  }
}
