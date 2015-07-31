package com.faacets.qalg
package algos

import scala.{specialized => sp}

import spire.algebra._
import spire.math._
import spire.syntax.all._

import algebra._
import syntax.all._

trait Rref[M] extends Any {
  def rref(m: M): RrefDecomposition[M]
}

object Rref {
  implicit def fromAlg[M](implicit pack: PackField.ForM[M, _]): Rref[M] = pack.MRref
}

trait MutableRref[M] extends Any with Rref[M] {
  implicit def M: MatBuild[M, _]

  def unsafeRref(m: M): RrefDecomposition[M]
  def rref(m: M): RrefDecomposition[M] = unsafeRref(M.copy(m))
}

object MutableRref {
  implicit def fromPack[M](implicit pack: PackFieldMutable.ForM[M, _]): MutableRref[M] = pack.MRref
}
