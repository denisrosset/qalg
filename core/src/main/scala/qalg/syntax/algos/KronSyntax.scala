package com.faacets
package qalg
package syntax
package algos

import scala.{specialized => sp}

import algebra._
import qalg.algos._

trait KronSyntax {
  def kron[T](ts: T*)(implicit T: Kron[T]): T =
    ts.tail.foldLeft(ts.head)(T.kron(_, _))
  def reverseKron[T](ts: T*)(implicit T: Kron[T]): T =
    kron[T](ts.reverse:_*)
}
