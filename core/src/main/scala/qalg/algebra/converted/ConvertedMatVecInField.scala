package com.faacets.qalg
package algebra
package converted

import scala.{specialized => sp}

trait ConvertedMatVecInField[M, V, @sp(Double, Long) A, J] extends Any
    with ConvertedMatVecInRing[M, V, A, J]
    with ConvertedMatInField[M, A, J]
    with MatVecInField[M, V, A] {
  def source: MatVecInField[M, V, J]
}
