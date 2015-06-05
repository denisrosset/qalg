package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait PackMVField[M, V, @sp(Double, Long) A] extends Any with PackVField[V, A] with PackMField[M, A] with PackMVEuclideanRing[M, V, A]
