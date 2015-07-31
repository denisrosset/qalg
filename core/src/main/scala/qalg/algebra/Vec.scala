package com.faacets.qalg
package algebra

import scala.{specialized => sp}

import indup.algebra.IndexAt1

trait Vec[V, @sp(Double, Long) A] extends Any with IndexAt1[V, V, A] {
  def length(v: V): Int = size(v)
  def feedTo(v: V, builder: VecBuilder[_, A]): Unit
}
