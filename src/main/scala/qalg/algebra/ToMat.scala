package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait ToMat[M1, M2] {
  def toMat(m1: M1): M2
}

object ToMat {
  implicit def create[M1, M2, @sp(Double, Long) A](implicit
    M1: Mat[M1, A],
    M2: MatBuilder[M2, A]): ToMat[M1, M2] = new ToMat[M1, M2] {
    def toMat(m1: M1): M2 = M2.fromFunM(M1.view(m1, ::, ::))
  }
}
