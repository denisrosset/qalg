package com.faacets.qalg
package algebra

import scala.{specialized => sp}
/*import spire.algebra._
import spire.syntax.eq._
import spire.syntax.cfor._
 import util._*/

trait VecBuilder[V, @sp(Double, Long) A] extends Any {
  def size: Int
  def add(i: Int, a: A): Unit
  def result(): V
}
