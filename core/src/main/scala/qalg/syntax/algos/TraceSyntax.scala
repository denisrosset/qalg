package com.faacets
package qalg
package syntax
package algos

import scala.{specialized => sp}

import algebra._
import qalg.algos._

trait TraceSyntax {
  implicit class TraceOps[M, @sp(Double, Long) A](m: M)(implicit M: Trace[M, A]) {
    def trace: A = M.trace(m)
  }
}
