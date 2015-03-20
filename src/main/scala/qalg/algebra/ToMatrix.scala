package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait ToMatrix[M1, M2] {
  def toMatrix(m1: M1): M2
}

object ToMatrix {
  implicit def create[M1, M2, @sp(Double, Long) A](implicit
    M1: Mat[M1, A],
    M2: MatBuilder[M2, A]): ToMatrix[M1, M2] = new ToMatrix[M1, M2] {
    def toMatrix(m1: M1): M2 = M2.from(M1.view(m1, ::, ::))
  }
}
