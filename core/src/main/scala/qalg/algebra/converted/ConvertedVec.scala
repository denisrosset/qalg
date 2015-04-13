package com.faacets.qalg
package algebra
package converted

import scala.{specialized => sp}

trait ConvertedVec[V, @sp(Double, Long) A, J] extends Any
    with Converted[A, J]
    with Vec[V, A] {
  def source: Vec[V, J]

  override def sameShape(x: V, y: V): Boolean = source.sameShape(x, y)
  override def linearLength(v: V): Int = length(v)
  override def linearApply(v: V, k: Int): A = apply(v, k)

  def length(v: V): Int = source.length(v)
  def apply(v: V, k: Int): A = jToA(source(v, k))
  override def toIndexedSeq(v: V): IndexedSeq[A] = new IndexedSeq[A] {
    def length: Int = source.length(v)
    def apply(k: Int): A = jToA(source(v, k))
  }
}
