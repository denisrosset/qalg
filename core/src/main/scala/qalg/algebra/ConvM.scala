package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait ConvM[IM, UM] extends Any {
  implicit def UM: MatMutable[UM, _]
  def toUM(m: IM): UM = UM.copy(unsafeToUM(m))
  def toIM(m: UM): IM = unsafeToIM(UM.copy(m))
  def unsafeToUM(m: IM): UM
  def unsafeToIM(m: UM): IM
}
