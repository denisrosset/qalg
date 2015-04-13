package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait ToVec[V1, V2] {
  def toVec(v1: V1): V2
}

object ToVec {
  implicit def create[V1, V2, @sp(Double, Long) A](implicit
    V1: Vec[V1, A],
    V2: VecBuilder[V2, A]): ToVec[V1, V2] = new ToVec[V1, V2] {
    def toVec(v1: V1): V2 = V2.tabulate(V1.length(v1))(k => V1(v1, k))
  }
}
