package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait PackMVEuclideanRing[M, V, @sp(Double, Long) A] extends Any with PackVEuclideanRing[V, A] with PackMEuclideanRing[M ,A] with PackMVRing[M, V, A]

