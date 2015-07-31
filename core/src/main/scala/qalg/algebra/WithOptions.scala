package com.faacets.qalg
package algebra

trait WithOptions[T] extends Any {
  /** Options type. Can specify object shape, storage mode, sparsity. */
  type Options

  /** Options used by default. */
  def defaultOptions: Options

  /** Recover options from object. */
  def options(t: T): Options

}
