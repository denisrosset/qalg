package com.faacets.qalg
package algebra

import scala.{specialized => sp}
import spire.algebra._
import spire.syntax.ring._
import spire.syntax.cfor._
import util._

trait MatVecMutable[M, V, @sp(Double, Long) A] extends Any
    with MatMutable[M, A] { self =>
  implicit def V: VecMutable[V, A]
}

trait ConvertedMatVecMutable[M, V, @sp(Double, Long) A, J] extends Any
    with MatVecMutable[M, V, A]
    with ConvertedMatMutable[M, A, J] {
  def source: MatVecMutable[M, V, J]
}

