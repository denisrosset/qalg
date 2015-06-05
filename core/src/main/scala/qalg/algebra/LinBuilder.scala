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
