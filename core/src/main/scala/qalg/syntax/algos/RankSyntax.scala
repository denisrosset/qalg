package com.faacets
package qalg
package syntax
package algos

import scala.{specialized => sp}

import algebra._
import qalg.algos._

trait RankSyntax {
  implicit class RankOps[M, @sp(Double, Long) A](m: M)(implicit M: Rank[M]) {
    def rank: Int = M.rank(m)
  }
}
