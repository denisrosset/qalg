package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait PackMVF[M, V, @sp(Double, Long) A] extends Any with PackVF[V, A] with PackMF[M, A] with PackMVR[M, V, A]
