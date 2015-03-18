package com.faacets.qalg
package algebra

import scala.language.higherKinds

import scala.{specialized => sp}
import spire.algebra._
import spire.syntax.cfor._
import util._

trait MatVecProduct[MA, VA, @sp(Double, Long) A] extends Any with Mat[MA, A] {
  implicit def VA: VecBuilder[VA, A]
  def timesl2(v: VA, m: MA): VA
  def timesr2(m: MA, v: VA): VA
}
