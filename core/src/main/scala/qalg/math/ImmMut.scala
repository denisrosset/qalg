package com.faacets.qalg
package math

sealed trait ImmMut
sealed trait Imm extends ImmMut
sealed trait Mut extends ImmMut
