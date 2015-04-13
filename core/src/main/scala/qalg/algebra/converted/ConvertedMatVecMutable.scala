package com.faacets.qalg
package algebra
package converted

import scala.{specialized => sp}

trait ConvertedMatVecMutable[M, V, @sp(Double, Long) A, J] extends Any
    with MatVecMutable[M, V, A]
    with ConvertedMatMutable[M, A, J] {
  def source: MatVecMutable[M, V, J]
}
