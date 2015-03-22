package com.faacets.qalg
package algebra

import scala.{specialized => sp}
import spire.algebra._
import spire.syntax.ring._
import spire.syntax.cfor._
import util._

trait MatVec[M, V, @sp(Double, Long) A] extends Any with Mat[M, A] { self =>
  implicit def V: Vec[V, A]
}

trait ConvertedMatVec[M, V, @sp(Double, Long) A, J] extends Any
    with ConvertedMat[M, A, J]
    with MatVec[M, V, A] {
  def source: MatVec[M, V, J]
}
