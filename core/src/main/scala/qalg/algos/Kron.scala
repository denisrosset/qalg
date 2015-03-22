package com.faacets.qalg
package algos

import spire.algebra._
import spire.math._
import spire.syntax.monoid._
import spire.syntax.vectorSpace._
import spire.syntax.cfor._

import algebra._
import syntax.all._

trait Kron {
  def kron[L](lins: L*)(implicit ev: Lin[L, _] with Monoid[L]): L =
    lins.tail.foldLeft(lins.head)(_ |+| _)

  def reverseKron[L](lins: L*)(implicit ev: Lin[L, _] with Monoid[L]): L =
    kron[L](lins.reverse:_*)
}
