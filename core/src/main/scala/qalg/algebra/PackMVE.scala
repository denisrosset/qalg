package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait PackMVE[M, V, @sp(Double, Long) A] extends Any with PackVE[V, A] with PackME[M ,A] with PackMVR[M, V, A]
