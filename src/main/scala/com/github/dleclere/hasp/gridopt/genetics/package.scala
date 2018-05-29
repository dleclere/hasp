package com.github.dleclere.hasp.gridopt
import com.github.dleclere.hasp.genetics._
import com.github.dleclere.hasp.genetics.base._

import scala.collection.{BitSet, SortedSet}
import scala.language.higherKinds
import com.github.dleclere.hasp.gridopt._
import BaseImps._
import com.github.dleclere.hasp.algorithms.scheduling.ccf.{CCF, CCFScheduleOps}
import com.github.dleclere.hasp.genetics.random.RandomNumberProvider
import scala.annotation.tailrec
import com.github.dleclere.hasp.utils._

import scala.math.Ordering

package object genetics {

  implicit def TplOrdering(implicit ord1: Ordering[Int], ord2: Ordering[Int]): Ordering[(Int, Int)] =
    (x: (Int, Int), y: (Int, Int)) =>
      ord1.compare(x._1, y._1)


  def taskSuccessors(head: TaskId, tasks: TaskId Map Task)(implicit rand: RandomNumberProvider): List[TaskId] = {

    def inner(id: TaskId): Seq[TaskId] =
      id +: rand.shuffle(tasks(id).successors).map(_.taskId).flatMap(inner)

    rand.shuffle(tasks(head).successors.map(_.taskId)).toList.flatMap(inner)
  }

  class GridGeneticsCCFScheduleOps(mkr: (Task, ScheduleLike[ScheduledTaskLike[TaskId]]) => Option[ScheduledTaskLike[TaskId]]) extends CCFScheduleOps[ScheduleLike, ScheduledTaskLike, TaskId] {

    override def isScheduled(id: TaskId, s: ScheduleLike[ScheduledTaskLike[TaskId]]): Boolean =
      s.taskSchedule.contains(id)

    override def schedule(id: TaskId, s: ScheduleLike[ScheduledTaskLike[TaskId]])(implicit rand: RandomNumberProvider): Option[ScheduleLike[ScheduledTaskLike[TaskId]]] =
      mkr(s.tasks(id), s).map(scheduleTask(_, s))

    override def children(id: TaskId, s: ScheduleLike[ScheduledTaskLike[TaskId]])(implicit rand: RandomNumberProvider): List[TaskId] =
      s.tasks(id).successors.map(_.taskId).toList

    override def noParents(s: ScheduleLike[ScheduledTaskLike[TaskId]])(implicit rand: RandomNumberProvider): Vector[TaskId] =
      s.tasks.values.filter(_.predecessors.isEmpty).map(_.taskId).toVector
  }

  implicit object ScheduleFitter extends Fitter[ScheduleLike, ScheduledTaskLike[TaskId]] {

    override def fitness(c: ScheduleLike[ScheduledTaskLike[TaskId]])(implicit utils: ChromosomeUtils[ScheduleLike]): Double =
      c.genes.maxBy(_.end.num).end.num.toDouble

  }

  implicit object ScheduleUtils extends ChromosomeUtils[ScheduleLike] {

    override def length[A](fa: ScheduleLike[A]): Int = fa.genes.size

    def apply[A](fa: ScheduleLike[A])(id: Int): A = fa.genes(id)

  }

  def genScheduledTask(task: Task, schedule: ScheduleLike[ScheduledTaskLike[TaskId]], resources: ResourceId Map Resource, latestFinish: Period, resourcePreference: Option[ResourceId])(implicit rand: RandomNumberProvider): Option[ScheduledTaskLike[TaskId]] =
    task.predecessors
      .map(schedule.taskSchedule.get)
      .map(_.map(_.end.num + 1))
      .foldLeft(Option(0)) { (lastMax, pred) =>
        pred.flatMap(p => lastMax.map(Math.max(_, p)))
      }
      .map(Period)
      .flatMap { earliestStart =>
        selectResourceSlot(earliestStart, latestFinish, task.work, resources, schedule.resourceSchedule, resourcePreference)
          .map {
            case (resource, start, finish) =>
              ScheduledTask(task.taskId, resource, start, finish)
          }
      }

  def selectResourceSlot(windowStart: Period, windowEnd: Period, work: Seq[Work], resources: ResourceId Map Resource, utilisation: ResourceId Map SortedSet[(Int, Int)], resourcePreference: Option[ResourceId])(implicit rand: RandomNumberProvider): Option[(ResourceId, Period, Period)] = {
    @tailrec
    def slotInner(resourceIds: Seq[ResourceId]): Option[(ResourceId, Period, Period)] = {
      resourceIds.headOption match {
        case None =>
          None

        case Some(targetResource) =>
          val untriedResources = resourceIds.drop(1)

          def periodRange(p: Period): Option[(Period, Period)] =
            firstAvailablePeriodRange(utilisation(targetResource), p, windowStart, windowEnd)

          val range = periodsToComplete(work, resources(targetResource)).flatMap { p =>
            val result = periodRange(p)
            result
          }
          if (range.nonEmpty || untriedResources.isEmpty)
            range.map { case (start, finish) =>
              (targetResource, start, finish)
            }
          else
            slotInner(untriedResources)

      }

    }

    val shuffled = rand.shuffle(resources.keys.toSeq)
    val result = slotInner(resourcePreference.fold(shuffled)(pref => pref +: shuffled.filterNot(_ == pref)))

    result
  }

  def firstAvailablePeriodRange(allocated: SortedSet[(Int, Int)], window: Period, earliestStart: Period, latestFinish: Period): Option[(Period, Period)] = {
    val es = earliestStart.num
    val lf = latestFinish.num
    val w = window.num
    @tailrec
    def rangeInner(end: Int, remaining: Array[(Int, Int)]): Option[(Period, Period)] = {
      val rs = remaining.drop(1)
      remaining.headOption match {

        case Some((r, _)) if end > es && end + w < lf && end != r - 1 && r - end > w =>
          Some((end + 1, end + 1 + w))

        case Some((r, ne)) =>
          rangeInner(ne, rs)

        case None =>
          val newStart = Math.max(end + 1, es)
          val newEnd = newStart + w
          if (newEnd <= lf) {
            Some((newStart, newEnd))
          } else {
            None
          }

      }
    }
    if (allocated.isEmpty)
      Some(earliestStart, Period(earliestStart.num + window.num))
    else {
      rangeInner(0, allocated.toArray)
    }

  }

  @inline
  def scheduleTask(task: ScheduledTaskLike[TaskId], schedule: ScheduleLike[ScheduledTaskLike[TaskId]]): ScheduleLike[ScheduledTaskLike[TaskId]] =
    schedule.withScheduledTask(task.id, task)

  @inline
  def unscheduleTask(taskId: TaskId, schedule: ScheduleLike[ScheduledTaskLike[TaskId]]): ScheduleLike[ScheduledTaskLike[TaskId]] =
    schedule.withoutScheduledTask(taskId)

  @inline
  def periodRange(start: Period, end: Period): (Int, Int) =
    (start.num, end.num)

  @inline
  def periodRange(task: ScheduledTaskLike[TaskId]): (Int, Int) =
    periodRange(task.start, task.end)

  @inline
  def isWithin(task: ScheduledTaskLike[TaskId], start: Period, end: Period): Boolean =
    task.start.num >= start.num && task.start.num <= end.num ||
      task.end.num <= end.num && task.end.num >= start.num

  def utilisingResourceWithin(schedule: ScheduleLike[ScheduledTaskLike[TaskId]], resource: ResourceId, start: Period, end: Period): Iterable[TaskId] =
    schedule.taskSchedule.values
      .filter(t => t.resource == resource && isWithin(t, start, end))
      .map(_.id)

}
