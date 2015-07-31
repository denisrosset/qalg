package com.faacets.qalg
package algebra

import scala.{specialized => sp}
import spire.algebra._
import spire.syntax.ring._
import spire.syntax.cfor._
import util._

trait MatVecProduct[M, V] extends Any { self =>
  def timesl2(v: V, m: M): V
  def timesr2(m: M, v: V): V
}

object MatVecProduct {
  def apply[M, V](implicit P: MatVecProduct[M, V]): MatVecProduct[M, V] = P
//  implicit def fromPack[M, V](implicit ev: PackMVR[M, V, _]): MatVecProduct[M, V] = ev.MVProduct
}
