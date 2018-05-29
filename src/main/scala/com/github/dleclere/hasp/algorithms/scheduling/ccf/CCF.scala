package com.github.dleclere.hasp.algorithms.scheduling.ccf

import com.github.dleclere.hasp.genetics.random.RandomNumberProvider
import com.github.dleclere.hasp.utils.remove

import scala.annotation.tailrec
import scala.language.higherKinds


trait CCFScheduleOps[S[_], T[_], Id] {

  def isScheduled(id: Id, s: S[T[Id]]): Boolean

  def schedule(id: Id, s: S[T[Id]])(implicit rand: RandomNumberProvider): Option[S[T[Id]]]

  def children(id: Id, s: S[T[Id]])(implicit rand: RandomNumberProvider):  List[Id]

  def noParents(s: S[T[Id]])(implicit rand: RandomNumberProvider): Vector[Id]

}

object CCF {

  def apply[Id, T[_], S[_]](sch: S[T[Id]])(implicit rand: RandomNumberProvider, ops: CCFScheduleOps[S, T, Id]): Option[S[T[Id]]] = {
    import ops._
    @tailrec
    def inner(queue: Vector[List[Id]], outstanding: Set[Id], s: S[T[Id]]): Option[S[T[Id]]] =
      if (queue.isEmpty) {
        Some(s).filter(_ => outstanding.isEmpty)
      } else {
        val ql = queue.length
        val i = rand.random(ql)
        queue(i) match {

          case t :: ts if !isScheduled(t, s) =>
            val next = schedule(t, s)
            inner(queue.updated(i, ts ++ children(t, s)), next.fold(outstanding + t)(_ => outstanding - t), next.getOrElse(s))

          case t :: ts =>
            inner(queue.updated(i, ts ++ children(t, s)), outstanding, s)

          case _ :: ts =>
            inner(queue.updated(i, ts), outstanding, s)

          case _ =>
            inner(remove(queue, i), outstanding, s)

        }

      }

    inner(noParents(sch).map(List(_)), Set.empty, sch)

  }


}
