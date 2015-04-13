package com.faacets.qalg
package algebra
package converted

import scala.{specialized => sp}

trait ConvertedMatVecInRing[M, V, @sp(Double, Long) A, J] extends Any
    with ConvertedMatVecBuilder[M, V, A, J]
    with ConvertedMatInRing[M, A, J]
    with MatVecInRing[M, V, A] {
  def source: MatVecInRing[M, V, J]
  override def timesl2(v: V, m: M): V = source.timesl2(v, m)
  override def timesr2(m: M, v: V): V = source.timesr2(m, v)
}
