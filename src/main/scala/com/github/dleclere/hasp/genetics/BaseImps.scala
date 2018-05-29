package com.github.dleclere.hasp.genetics

import java.util.concurrent.ThreadLocalRandom

import com.github.dleclere.hasp.genetics.random.RandomNumberProvider
import breeze.stats.distributions.RandBasis

import scala.collection.GenIterable
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scala.math.Ordering
import scala.reflect.ClassTag
import scala.util.Random
import scala.collection.immutable.IndexedSeq
import scala.collection.parallel.immutable.ParVector

object BaseImps {
  import base._

  implicit object TempRandomNumberGen extends RandomNumberProvider {

    override val seed: Int = 1

    private def gen: Random = ThreadLocalRandom.current()

    override def random(maxExclusive: Int): Int = gen.nextInt(maxExclusive)

    override def nextFloat: Float = gen.nextFloat()

    override def nextDouble: Double = gen.nextDouble()

    override def shuffle[T, CC[X] <: TraversableOnce[X]](xs: CC[T])(implicit bf: CanBuildFrom[CC[T], T, CC[T]]): CC[T] =
      gen.shuffle(xs)

    override def basis: RandBasis = RandBasis.withSeed(seed)

  }

  implicit object VecCollectionUtils extends CollectionUtils[Vector] {

    def const[A](a: A*): Vector[A] = Vector(a:_*)

    def apply[A](fa: Vector[A])(id: Int): A = fa(id)

    override def map[A, B](fa: Vector[A])(f: A => B): Vector[B] = fa.map(f)

    override def flatMap[A, B](fa: Vector[A])(f: A => Vector[B]): Vector[B] = fa.flatMap(f)

    override def foldLeft[A, B](fa: Vector[A])(z: B)(op: (B, A) => B): B = fa.foldLeft(z)(op)

    override def get[A](fa: Vector[A])(id: Int): Option[A] = if (fa isDefinedAt id) Some(fa(id)) else None

    override def length[A](fa: Vector[A]): Int = fa.length

    override def sortBy[A, B](fa: Vector[A])(f: A => B)(implicit ord: Ordering[B]): Vector[A] = fa.sortBy(f)

    override def zip[A, B, That](fa: Vector[A])(that: GenIterable[B]): Vector[(A, B)] = fa.zip(that)

    override def toArray[A, B >: A : ClassTag](fa: Vector[A]): Array[B] = fa.toArray

    override def append[A](fa: Vector[A])(a: A): Vector[A] = fa :+ a

    def filter[A](fa: Vector[A])(f: A => Boolean): Vector[A] = fa.filter(f)

    override def toSeq[A](fa: Vector[A]): Seq[A] = fa

    override def empty[A]: Vector[A] = Vector.empty[A]

    override def take[A](fa: Vector[A])(num: Int): Vector[A] = fa.take(num)

    override def reverse[A](fa: Vector[A]): Vector[A] = fa.reverse

    override def prepend[A](fa: Vector[A])(a: A): Vector[A] = a +: fa
  }
  
  implicit object ParVecCollectionUtils extends CollectionUtils[ParVector] {

    def const[A](a: A*): ParVector[A] = ParVector(a:_*)

    def apply[A](fa: ParVector[A])(id: Int): A = fa(id)

    override def map[A, B](fa: ParVector[A])(f: A => B): ParVector[B] = fa.map(f)

    override def flatMap[A, B](fa: ParVector[A])(f: A => ParVector[B]): ParVector[B] = fa.flatMap(f)

    override def foldLeft[A, B](fa: ParVector[A])(z: B)(op: (B, A) => B): B = fa.foldLeft(z)(op)

    override def get[A](fa: ParVector[A])(id: Int): Option[A] = if (fa isDefinedAt id) Some(fa(id)) else None

    override def length[A](fa: ParVector[A]): Int = fa.length

    override def sortBy[A, B](fa: ParVector[A])(f: A => B)(implicit ord: Ordering[B]): ParVector[A] = fa.seq.sortBy(f).par

    override def zip[A, B, That](fa: ParVector[A])(that: GenIterable[B]): ParVector[(A, B)] = fa.zip(that)

    override def toArray[A, B >: A : ClassTag](fa: ParVector[A]): Array[B] = fa.toArray

    override def append[A](fa: ParVector[A])(a: A): ParVector[A] = fa :+ a

    def filter[A](fa: ParVector[A])(f: A => Boolean): ParVector[A] = fa.filter(f)

    override def toSeq[A](fa: ParVector[A]): Seq[A] = fa.seq

    override def empty[A]: ParVector[A] = ParVector.empty[A]

    override def take[A](fa: ParVector[A])(num: Int): ParVector[A] = fa.take(num)

    override def reverse[A](fa: ParVector[A]): ParVector[A] = fa.reverse

    override def prepend[A](fa: ParVector[A])(a: A): ParVector[A] = a +: fa
  }
  
  implicit object SeqCollectionUtils extends CollectionUtils[Seq] {

    def const[A](a: A*): Seq[A] = Seq(a:_*)

    def apply[A](fa: Seq[A])(id: Int): A = fa(id)

    override def map[A, B](fa: Seq[A])(f: A => B): Seq[B] = fa.map(f)

    override def flatMap[A, B](fa: Seq[A])(f: A => Seq[B]): Seq[B] = fa.flatMap(f)

    override def foldLeft[A, B](fa: Seq[A])(z: B)(op: (B, A) => B): B = fa.foldLeft(z)(op)

    override def get[A](fa: Seq[A])(id: Int): Option[A] = if (fa isDefinedAt id) Some(fa(id)) else None

    override def length[A](fa: Seq[A]): Int = fa.length

    override def sortBy[A, B](fa: Seq[A])(f: A => B)(implicit ord: Ordering[B]): Seq[A] = fa.sortBy(f)

    override def zip[A, B, That](fa: Seq[A])(that: GenIterable[B]): Seq[(A, B)] = fa.zip(that)

    override def toArray[A, B >: A : ClassTag](fa: Seq[A]): Array[B] = fa.toArray

    override def append[A](fa: Seq[A])(a: A): Seq[A] = fa :+ a

    def filter[A](fa: Seq[A])(f: A => Boolean): Seq[A] = fa.filter(f)

    override def toSeq[A](fa: Seq[A]): Seq[A] = fa

    override def empty[A]: Seq[A] = Seq.empty[A]

    override def take[A](fa: Seq[A])(num: Int): Seq[A] = fa.take(num)

    override def reverse[A](fa: Seq[A]): Seq[A] = fa.reverse

    override def prepend[A](fa: Seq[A])(a: A): Seq[A] = a +: fa
  }
  
  implicit object IndexedSeqCollectionUtils extends CollectionUtils[IndexedSeq] {

    def const[A](a: A*): IndexedSeq[A] = IndexedSeq(a:_*)

    def apply[A](fa: IndexedSeq[A])(id: Int): A = fa(id)

    override def map[A, B](fa: IndexedSeq[A])(f: A => B): IndexedSeq[B] = fa.map(f)

    override def flatMap[A, B](fa: IndexedSeq[A])(f: A => IndexedSeq[B]): IndexedSeq[B] = fa.flatMap(f)

    override def foldLeft[A, B](fa: IndexedSeq[A])(z: B)(op: (B, A) => B): B = fa.foldLeft(z)(op)

    override def get[A](fa: IndexedSeq[A])(id: Int): Option[A] = if (fa isDefinedAt id) Some(fa(id)) else None

    override def length[A](fa: IndexedSeq[A]): Int = fa.length

    override def sortBy[A, B](fa: IndexedSeq[A])(f: A => B)(implicit ord: Ordering[B]): IndexedSeq[A] = fa.sortBy(f)

    override def zip[A, B, That](fa: IndexedSeq[A])(that: GenIterable[B]): IndexedSeq[(A, B)] = fa.zip(that)

    override def toArray[A, B >: A : ClassTag](fa: IndexedSeq[A]): Array[B] = fa.toArray

    override def append[A](fa: IndexedSeq[A])(a: A): IndexedSeq[A] = fa :+ a

    def filter[A](fa: IndexedSeq[A])(f: A => Boolean): IndexedSeq[A] = fa.filter(f)

    override def toSeq[A](fa: IndexedSeq[A]): IndexedSeq[A] = fa

    override def empty[A]: IndexedSeq[A] = IndexedSeq.empty[A]

    override def take[A](fa: IndexedSeq[A])(num: Int): IndexedSeq[A] = fa.take(num)

    override def reverse[A](fa: IndexedSeq[A]): IndexedSeq[A] = fa.reverse

    override def prepend[A](fa: IndexedSeq[A])(a: A): IndexedSeq[A] = a +: fa

  }

  implicit object ListCollectionUtils extends CollectionUtils[List] {

    def const[A](a: A*): List[A] = List(a:_*)

    def apply[A](fa: List[A])(id: Int): A = fa(id)

    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)

    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)

    override def foldLeft[A, B](fa: List[A])(z: B)(op: (B, A) => B): B = fa.foldLeft(z)(op)

    override def get[A](fa: List[A])(id: Int): Option[A] = if (fa isDefinedAt id) Some(fa(id)) else None

    override def length[A](fa: List[A]): Int = fa.length

    override def sortBy[A, B](fa: List[A])(f: A => B)(implicit ord: Ordering[B]): List[A] = fa.sortBy(f)

    override def zip[A, B, That](fa: List[A])(that: GenIterable[B]): List[(A, B)] = fa.zip(that)

    override def toArray[A, B >: A : ClassTag](fa: List[A]): Array[B] = fa.toArray

    override def append[A](fa: List[A])(a: A): List[A] = fa :+ a

    def filter[A](fa: List[A])(f: A => Boolean): List[A] = fa.filter(f)

    override def toSeq[A](fa: List[A]): List[A] = fa

    override def empty[A]: List[A] = List.empty[A]

    override def take[A](fa: List[A])(num: Int): List[A] = fa.take(num)

    override def reverse[A](fa: List[A]): List[A] = fa.reverse

    override def prepend[A](fa: List[A])(a: A): List[A] = a +: fa
  }

}
