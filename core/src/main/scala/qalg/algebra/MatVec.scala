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

object MatVec {
  def apply[M, V, @sp(Double, Long) A](implicit MV: MatVec[M, V, A]): MatVec[M, V, A] = MV
}
