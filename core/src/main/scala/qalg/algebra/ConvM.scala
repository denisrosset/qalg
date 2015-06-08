package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait ConvM[IM, UM, @sp(Double, Long) A] extends Any {
  def toUM(m: IM): UM
  def toIM(m: UM): IM
  def unsafeToUM(m: IM): UM
  def unsafeToIM(m: UM): IM
}
