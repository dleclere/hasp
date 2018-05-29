package com.github.dleclere.hasp

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all._
import cats.effect._
import cats.instances.list._
import cats.syntax.parallel._
import cats.syntax._


package object utils {

  def applyTpl[T, V, R](tpl: (T, V))(fn: (T, V) => R): R =
    fn(tpl._1, tpl._2)


  def insert[T](list: Vector[T], i: Int, value: T): Vector[T] = {
    val (front, back) = list splitAt i
    front ++ Vector(value) ++ back
  }

  def remove[T](list: Vector[T], i: Int): Vector[T] = {
    val (front, back) = list splitAt i
      front ++ back.drop(1)
  }

  def insertBefore[T](list: Vector[T], t: T, before: T): Vector[T] =
    list.foldLeft(Vector.empty[T]) {

      case (v, `before`) =>
        v :+ t :+ before

      case (v, e) =>
        v :+ e

    }

  def insertAfter[T](list: Vector[T], t: T, after: T): Vector[T] =
    list.foldLeft(Vector.empty[T]) {

      case (v, `after`) =>
        v :+ after :+t

      case (v, e) =>
        v :+ e

    }

  implicit class ExtEither[+A, +B](either: Either[A, B]) {

    @inline final def orElse[A2 >: A, B2 >: B](alternative: => Either[A2, B2]): Either[A2, B2] =
      if (either.isLeft) alternative else either

  }

  def parIos[T](ios: Seq[IO[T]]): IO[Seq[T]] =
    //NonEmptyList.fromList(ios.toList).fold(IO.pure(Seq.empty[T]))(_.parSequence.map(_.toList))
    ios.foldLeft(IO.pure(Seq.empty[T])) { (m, t) =>
      t.flatMap(t2 => m.map(_ :+ t2))
    }

  def mkIdMap[Id, T](extractor: T => Id)(seq: Seq[T]): Id Map T =
    seq.map(s => extractor(s) -> s).toMap


  def liftOptionSeq[T](seq: Iterable[Option[T]]): Option[Iterable[T]] =
    seq.foldLeft(Option(Seq.empty[T])) { (m, t) =>
      t.flatMap(t2 => m.map(_ :+ t2))
    }

  def eitherError[E <: Throwable, S](opt: Option[S])(err: => E): Either[E, S] =
    opt.fold[Either[E, S]](Left(err))(Right(_))

  def eitherErrorv2[S, E <: Throwable](opt: Option[S], err: => E): Either[E, S] =
    opt.fold[Either[E, S]](Left(err))(Right(_))

}
