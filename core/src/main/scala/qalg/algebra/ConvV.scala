package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait ConvV[IV, UV, @sp(Double, Long) A] extends Any {
  def toUV(v: IV): UV
  def toIV(v: UV): IV
  def unsafeToUV(v: IV): UV
  def unsafeToIV(v: UV): IV
}
