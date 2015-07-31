package com.faacets.qalg
package algos

import scala.{specialized => sp}

import spire.algebra._
import spire.math._
import spire.syntax.all._

import algebra._
import syntax.all._
import util._

trait RrefDecomposition[M] extends Any { self =>
  def reduced: M
  def basis: Array[Int]
  def rank: Int = basis.length
  def map[M1](f: M => M1) = new RrefDecomposition[M1] {
    val reduced = f(self.reduced)
    def basis = self.basis
  }
}
