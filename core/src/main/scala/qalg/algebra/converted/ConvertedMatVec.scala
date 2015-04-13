package com.faacets.qalg
package algebra
package converted

import scala.{specialized => sp}

trait ConvertedMatVec[M, V, @sp(Double, Long) A, J] extends Any
    with ConvertedMat[M, A, J]
    with MatVec[M, V, A] {
  def source: MatVec[M, V, J]
}
