package com.faacets.qalg
package algebra

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.eq._
import util._

trait LinBuilder[LA, @sp(Double, Long) A] extends Any with Lin[LA, A] { self =>
  def map(l: LA)(f: A => A): LA
}

object LinBuilder {
  def apply[LA, @sp(Double, Long) A](implicit ev: LinBuilder[LA, A]): LinBuilder[LA, A] = ev
  implicit def fromMatPack[M, @sp(Double, Long) A](implicit ev: PackMR[M, A]): LinBuilder[M, A] = ev.M
  implicit def fromVecPack[V, @sp(Double, Long) A](implicit ev: PackVR[V, A]): LinBuilder[V, A] = ev.V
}
