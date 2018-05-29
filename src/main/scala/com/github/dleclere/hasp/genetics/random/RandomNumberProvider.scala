package com.github.dleclere.hasp.genetics.random

import breeze.stats.distributions.RandBasis

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

trait RandomNumberProvider {

  val seed: Int

  @deprecated
  def random(maxExclusive: Int): Int

  @deprecated
  def nextFloat: Float

  @deprecated
  def nextDouble: Double

  @deprecated
  def shuffle[T, CC[X] <: TraversableOnce[X]](xs: CC[T])(implicit bf: CanBuildFrom[CC[T], T, CC[T]]): CC[T]

  @deprecated
  def basis: RandBasis

}