package com.github.dleclere.hasp.genetics

import com.github.dleclere.hasp.genetics.base.CoreCollectionUtils
import breeze.stats.distributions.Rand

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.math._

package object random {


//  def random[@specialized(Int, Double) T](maxExclusive: T)(implicit r: Rand[T]): T =
//
//
//  def nextFloat(implicit p: RandomNumberProvider): Float
//
//  def shuffle[T, CC[X] <: TraversableOnce[X]](xs: CC[T])(implicit bf: CanBuildFrom[CC[T], T, CC[T]],  p: RandomNumberProvider): CC[T]

  @tailrec
  final def weightedSelector[S[_], A](weighted: S[(A, Double)])(implicit gen: RandomNumberProvider, utils: CoreCollectionUtils[S]): A =
    utils(weighted)(gen.random(utils.length(weighted))) match {
      case (s, f) if f > gen.nextFloat => s
      case _ => weightedSelector(weighted)
    }


  def expRandom(rate: Double, min: Double, max: Double)(implicit gen: RandomNumberProvider): Double = {
    def e(x: Double) = exp(-rate * x)
    -log(e(min) - gen.nextDouble * (e(min) - e(max)))/rate
  }


}
