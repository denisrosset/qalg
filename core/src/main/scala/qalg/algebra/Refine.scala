package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait Base[V, B]

trait Refine[B, @sp(Double, Long) A, V]
