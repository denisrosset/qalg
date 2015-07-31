package com.faacets.qalg
package algebra

import scala.{specialized => sp}

import indup.algebra.IndexAt2

trait Mat[M, @sp(Double, Long) A] extends Any with IndexAt2[M, M, Nothing, A] { self =>
  def nRows(m: M): Int = size0(m)
  def nCols(m: M): Int = size1(m)
}
