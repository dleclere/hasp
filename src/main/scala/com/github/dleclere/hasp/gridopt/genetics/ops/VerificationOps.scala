package com.github.dleclere.hasp.gridopt.genetics.ops

import com.github.dleclere.hasp.gridopt.TaskId
import com.github.dleclere.hasp.gridopt.genetics.{ScheduleLike, ScheduledTask, ScheduledTaskLike, periodRange}
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge

import scala.collection.BitSet

object VerificationOps {


  def verifySchedule(graph: Graph[TaskId, DiEdge], schedule: ScheduleLike[ScheduledTaskLike[TaskId]]): Boolean = {
    verifyDependencies(graph, schedule) && verifyResourceAllocation(schedule)
  }

  def verifyResourceAllocation(schedule: ScheduleLike[ScheduledTaskLike[TaskId]]): Boolean = {
//    schedule.taskSchedule
//      .values
//      .groupBy(_.resource)
//      .mapValues {
//        _.map {
//          case task@ScheduledTask(_, _, start, end) =>
//            (task, periodRange(start, end))
//        }.foldLeft((true, BitSet.empty)) {
//          case ((true, total), (task, range)) =>
//            val canProceed = (total & range).isEmpty
//            if (!canProceed)
//              throw new Error(s"Resource allocation confict for $task in $total")
//            else
//              (canProceed, total ++ range)
//
//          case ((false, _), _) =>
//            (false, BitSet.empty)
//        }._1
//      }.forall(_._2 == true)
    true
  }

  def verifyDependencies(graph: Graph[TaskId, DiEdge], schedule: ScheduleLike[ScheduledTaskLike[TaskId]]): Boolean =
    graph.edges
      .forall { case graph.EdgeT(left, right) =>
        val result = schedule.taskSchedule(left.value).end.num < schedule.taskSchedule(right.value).start.num
        if (result == false)
          throw new Error(s"Task ends after successor begins ${schedule.taskSchedule(left.value)} > ${schedule.taskSchedule(right.value)}")
        else
          result
      }


}
