package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait Converted[@sp(Double, Long) A, J] extends Any {
  def aToJ(a: A): J
  def jToA(j: J): A
}
