package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait ConvV[IV, UV] extends Any {
  implicit def UV: VecMutable[UV, _]
  def toUV(v: IV): UV = UV.copy(unsafeToUV(v))
  def toIV(v: UV): IV = unsafeToIV(UV.copy(v))
  def unsafeToUV(v: IV): UV
  def unsafeToIV(v: UV): IV
}
