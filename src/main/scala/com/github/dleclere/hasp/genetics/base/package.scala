package com.github.dleclere.hasp.genetics

import com.github.dleclere.hasp.genetics.random._
import breeze.stats.distributions.RandBasis

import scala.annotation.tailrec
import scala.collection.GenIterable
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scala.math.Ordering
import scala.reflect.ClassTag


package object base {

  trait GenChromosome[Chromosome[_], Gene] {

    def apply()(implicit rand: RandomNumberProvider): Option[Chromosome[Gene]]

  }

  trait Fitter[Chromosome[_], Gene] {

    def fitness(c: Chromosome[Gene])(implicit utils: ChromosomeUtils[Chromosome]): Double

  }

  trait RandomMutation[Chromosome[_], Gene] {

    def randomMutate(chromosome: Chromosome[Gene], target: Int)(implicit utils: ChromosomeUtils[Chromosome], gen: RandomNumberProvider): Option[Chromosome[Gene]]

  }

  trait CrossoverMutation[Chromosome[_], Gene] {

    def crossoverMutate(src: Gene, dest: Chromosome[Gene], index: Int)(implicit utils: ChromosomeUtils[Chromosome], gen: RandomNumberProvider): Option[Chromosome[Gene]]

  }

  trait CoreCollectionUtils[P[_]] {

    def length[A](fa: P[A]): Int

    def apply[A](fa: P[A])(id: Int): A

  }

  trait CollectionUtils[P[_]] extends CoreCollectionUtils[P] {

    def const[A](a: A*): P[A]

    def map[A, B](fa: P[A])(f: A => B): P[B]

    def flatMap[A, B](fa: P[A])(f: A => P[B]): P[B]

    def foldLeft[A,B](fa: P[A])(z: B)(op: (B, A) => B): B

    def get[A](fa: P[A])(id: Int): Option[A]

    def sortBy[A, B](fa: P[A])(f: A => B)(implicit ord: Ordering[B]): P[A]

    def zip[A, B, That](fa: P[A])(that: GenIterable[B]): P[(A, B)]

    def toArray[A, B >: A : ClassTag](fa: P[A]): Array[B]

    def append[A](fa: P[A])(a: A): P[A]

    def prepend[A](fa: P[A])(a: A): P[A]

    def filter[A](fa: P[A])(f: A => Boolean): P[A]

    def filterNot[A](fa: P[A])(f: A => Boolean): P[A] =
      filter(fa)(a => !f(a))

    def toSeq[A](fa: P[A]): Seq[A]

    def empty[A]: P[A]

    def take[A](fa: P[A])(num: Int): P[A]

    def reverse[A](fa: P[A]): P[A]

  }

  trait ChromosomeUtils[C[_]] extends CoreCollectionUtils[C] {



  }

  def applyTillSome[T](f: => Option[T]): T = f match {

    case Some(t) =>
      t

    case _ =>
      applyTillSome(f)

  }

  def random[Provider[_], F[_], Gene](provider: Provider[F[Gene]])(implicit gen: RandomNumberProvider, utils: CollectionUtils[Provider]): F[Gene] =
    utils.get(provider)(gen.random(utils.length(provider))).get

  @tailrec
  final def monteCarlo[S[_], A](weighted: S[(A, Double)])(implicit gen: RandomNumberProvider, utils: CollectionUtils[S]): A = {
    utils(weighted)(gen.random(utils.length(weighted))) match {
      case (s, f) if f > gen.nextFloat => s
      case _ => monteCarlo(weighted)
    }
  }


}
