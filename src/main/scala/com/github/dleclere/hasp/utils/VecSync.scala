package com.github.dleclere.hasp.utils

object VecSync {
  sealed trait SyncOp[T] {

    val item: T

  }
  case class MoveBefore[T](item: T, before: T) extends SyncOp[T]
  case class MoveAfter[T](item: T, after: T) extends SyncOp[T]
  case class Remove[T](item: T) extends SyncOp[T]
  case class Append[T](item: T) extends SyncOp[T]

  def syncOps[T](orig: Vector[T], next: Vector[T]): Seq[SyncOp[T]] =
    next.zipWithIndex.foldLeft {
      (orig diff next)
        .foldLeft((orig, Seq.empty[SyncOp[T]])) { case ((m, ops), r) =>
          (m.filterNot(_ == r), ops :+ Remove(r))
        }
    } {

      case ((m, ops), (t, i)) if m.indexOf(t) != i =>
        val mf = m.filterNot(_ == t)
        if (m.isEmpty)
          (mf :+ t, ops :+ Append(t))
        else if (i >= m.length - 1)
          (insertAfter(mf, t, m.last), ops :+ MoveAfter(t, m.last))
        else
          (insertBefore(mf, t, m(i)), ops :+ MoveBefore(t, m(i)))

       case ((m, ops), (t, i)) =>
        (m, ops)

    }._2

  def applySyncOps[T](target: Vector[T], ops: Seq[SyncOp[T]]): Vector[T] =
    ops.foldLeft(target) {

      case (m, MoveBefore(t, b)) =>
        insertBefore(m.filterNot(_ == t), t, b)

      case (m, MoveAfter(t, a)) =>
        insertAfter(m.filterNot(_ == t), t, a)

      case (m, Remove(t)) =>
        m.filterNot(_ == t)

      case (m, Append(t)) =>
        m :+ t

    }

}
