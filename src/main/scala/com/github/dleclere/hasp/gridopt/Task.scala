package com.github.dleclere.hasp.gridopt
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits._
import scalax.collection.edge.WDiEdge

import io.circe.generic.JsonCodec
import io.circe.syntax._
import io.circe.generic.semiauto._
import io.circe.generic.extras._

trait TaskLike {
  val taskId: TaskId
  val work: Seq[Work]
}

trait AssignedTask extends TaskLike {

  val resource: ResourceId

}

object TaskBuilder {

  def buildTask(taskId: Int, baseline: Int)(successors: TaskSuccessor*): (TaskId, Seq[TaskId] => Task, Seq[TaskSuccessor]) = {
    (TaskId(taskId), preds => Task(taskId, baseline)(successors: _*)(preds: _*), successors)
  }

  def apply(builders: (TaskId, Seq[TaskId] => Task, Seq[TaskSuccessor])*): Seq[Task] = {
    val preds = builders
      .foldLeft(Map.empty[TaskId, Seq[TaskId]].withDefaultValue(Seq.empty)) { case (m, (t, _, ts)) =>
        ts
          .map(_.taskId)
          .foldLeft(m){ case (mp, s) =>  mp + (s -> (mp(s) :+ t)) }
      }
    builders.map { case (id, builder, _) =>
      builder(preds(id))
    }
  }
}

object Task {

  def apply(taskId: Int, baseline: Int)(successors: TaskSuccessor*)(predecessors: TaskId*): Task =
    Task(TaskId(taskId), Seq(Work(WorkTypeTag("default"), WorkAmount(baseline))), successors, predecessors)

}


case class Task(taskId: TaskId, work: Seq[Work], successors: Seq[TaskSuccessor], predecessors: Seq[TaskId]) extends TaskLike {

  def baseline: Int = work.map(_.amount.value).sum

}

object TaskSuccessor {

  def apply(taskId: Int, transitionCost: Int): TaskSuccessor =
    TaskSuccessor(TaskId(taskId), transitionCost)

}

case class TaskSuccessor(taskId: TaskId, transitionCost: Int)

object ScheduledTask {

  def apply(task: Task, resource: ResourceId, start: Period): ScheduledTask =
    ScheduledTask(task.taskId, task.work, resource, start)

}

case class ScheduledTask(taskId: TaskId, work: Seq[Work], resource: ResourceId, start: Period) extends AssignedTask

trait TaskOps {

  def timelessAllocationGraph(graph: Graph[TaskId, DiEdge], allocations: Seq[AssignedTask], resources: Seq[Resource]): Option[Graph[TaskId, WDiEdge]] = {
    val rMap = resources.map(r => r.id -> r).toMap
    val aMap = allocations.map(a => a.taskId -> a).toMap

    def cost(taskId: TaskId): Option[Int] = aMap.get(taskId).flatMap(a => rMap.get(a.resource).flatMap(periodsToComplete(a.work, _))).map(_.num)

    graph.edges.map {

      case graph.EdgeT(a, b) =>
        (if (graph.nodes.get(a).hasPredecessors) Some(0) else cost(a))
          .flatMap { initCost =>
            cost(b)
              .map(initCost + _)
              .map(t =>  a.value ~> b.value % t)
          }

    }.foldLeft[Option[Seq[WDiEdge[TaskId]]]](Option(Seq.empty[WDiEdge[TaskId]]))( (seq, edge) =>
      seq.flatMap(s => edge.map(s :+ _))
    ).map(s => Graph(s:_*))
  }

  def simpleTaskGraph(tasks: Task*): Graph[TaskId, DiEdge] =
    Graph(tasks.flatMap(t => t.successors.map(_.taskId).map(t.taskId ~> _)): _*)

}
