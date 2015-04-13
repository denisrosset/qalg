package com.faacets.qalg
package algebra
package converted

import scala.{specialized => sp}

trait ConvertedVecInField[V, @sp(Double, Long) A, J] extends Any
    with ConvertedVecInRing[V, A, J]
    with VecInField[V, A] {
  def source: VecInField[V, J]
}
