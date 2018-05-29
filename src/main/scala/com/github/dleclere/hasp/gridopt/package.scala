package com.github.dleclere.hasp
import io.circe.generic.JsonCodec
import io.circe.syntax._
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits._
import scalax.collection.edge.WDiEdge

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import com.github.dleclere.hasp.utils._
import io.circe.{Decoder, DecodingFailure, Encoder, Json}

package object gridopt extends PeriodOps with ExecutionOps with TaskOps with ResourceOps {

  implicit val encodeERPP = Encoder.instance[WorkTypeTag Map WorkAmount] { workMap => {
      import Json._
      fromFields {
        workMap.toSeq.map { case (key, value) =>
          (key.name, fromInt(value.value))
        }
      }
    }
  }

  implicit val decodeERRP = Decoder.instance[WorkTypeTag Map WorkAmount] { workMap =>
     Right[DecodingFailure, WorkTypeTag Map WorkAmount] {
       workMap.value.
         asObject
         .get
         .toMap
         .map { case (key, value) =>
           WorkTypeTag(key) ->
             value.asNumber
               .flatMap(_.toInt)
               .map(WorkAmount(_))
               .get

         }
     }

  }

  @JsonCodec case class TaskId(id: Int)

  @JsonCodec case class WorkTypeTag(name: String)

  @JsonCodec case class WorkAmount(value: Int)

  @JsonCodec case class Work(tag: WorkTypeTag, amount: WorkAmount)

  @JsonCodec case class ResourceId(id: Int)

  @JsonCodec case class ExecRatePerPeriod(capacity: WorkTypeTag Map WorkAmount)

  @JsonCodec case class Resource(id: ResourceId, execRatePerPeriod: ExecRatePerPeriod)

  def tasks(weight: Int*): Map[TaskId, Int] =
    weight.zipWithIndex.map { case (w, id) => TaskId(id) -> w }.toMap

  def task(id: Int, weight: Int, dependsOn: TaskId*): (TaskId, Int, Seq[TaskId]) =
    (TaskId(id), weight, dependsOn)

  def weightedEdges(task: Task): Seq[WDiEdge[TaskId]] =
    task.successors.map(s => task.taskId ~> s.taskId % (task.baseline + s.transitionCost))

  def findPaths(src: TaskId, target: TaskId, edges: TaskId => Seq[TaskId]): Seq[Seq[TaskId]] = {

    def inner(n: TaskId, path: Seq[TaskId]): Seq[Seq[TaskId]] =
      n match {

        case `target` =>
          Seq(path)

        case other =>
          edges(n).flatMap(t => inner(t, path :+ t))

      }

    inner(src, Seq(src))
  }

  def sumPath(path: Seq[TaskId], taskWeight: TaskId => Int, comWeight: TaskId => TaskId => Int): Int =
    path.foldLeft[(Int, TaskId => Int)]((0, _ => 0)) {

      case ((sum, comCost), task) =>
        (sum + taskWeight(task) + comCost(task), comWeight(task))

    }._1

  def findLongestPath(paths: Seq[Seq[TaskId]], taskWeight: TaskId => Int, comWeight: TaskId => TaskId => Int): Option[Seq[TaskId]] =
    paths
      .toIterator
      .map(path => (path, sumPath(path, taskWeight, comWeight)))
      .reduceOption[(Seq[TaskId], Int)] {

      case (a@(_, al), b@(_, bl)) if bl > al =>
        b

      case (a, _) =>
        a

    }.map(_._1)


  def run(tasks: Task*) = new {

    val vs = tasks.map(_.taskId)

    val es = tasks.flatMap { case Task(task, _, successors, _) => successors.map(task -> _) }

    val esMap = es.foldLeft(Map.empty[TaskId, Seq[TaskId]].withDefaultValue(Seq.empty)) {
      case (m, (t1, TaskSuccessor(t2, _))) => m + (t1 -> (m(t1) :+ t2))
    }

    val graph = Graph(tasks.flatMap(weightedEdges): _*)

    val sourceNodes = graph.nodes.filterNot(_.hasPredecessors).map(_.value).toSeq

    val endNodes = graph.nodes.filterNot(_.hasSuccessors).map(_.value).toSeq

    // Execution time
    val c: TaskId => Int = tasks.map { case task@Task(id, _, _, _) => id -> task.baseline }.toMap

    // Task Communication Time
    def τ: TaskId => TaskId => Int = t1 => t2 =>
      tasks
        .find(_.taskId == t1)
        .flatMap(_.successors.find(_.taskId == t2))
        .map(_.transitionCost)
        .getOrElse(0)

    // Task Start Time
    def st: TaskId => Int = ???

    // Task Finish Time
    def ft: TaskId => Int = ???

    def makespan: Int = ft(vs.maxBy(ft))

    //(top-level) for a node u is the weight of the longest path from the source node to u.
    val tlevel: TaskId => Int =
      vs.map(t =>
        t ->
          findLongestPath(sourceNodes.flatMap(findPaths(t, _, esMap.apply)), c, τ)
            .fold(0)(sumPath(_, c, τ))
      ).toMap.apply

    //- blevel (bottom-level) for a node u is the weight of the longest path from u to an exit node.
    val blevel: TaskId => Int =
      vs.map(t =>
        t ->
          findLongestPath(endNodes.flatMap(findPaths(t, _, esMap.apply)), c, τ)
            .fold(0)(sumPath(_, c, τ))
      ).toMap.apply

    //We define the ALAP (As Late As Possible) attribute for a node u to measure how far the node’s start-time, st(u) can be delayed without increasing the makespan.
    def alap: TaskId => Int = ???


    //    /**
    //      * computes tlevel and blevel for each node;
    //      * insert source task into RUNNING-QUEUE;
    //      * while (! isEmpty (RUNNING-QUEUE))
    //        * task = deq (RUNNING-QUEUE);
    //        * for-each child of task do
    //          * enq (child, CHILDREN-QUEUE);
    //        * end for
    //        * while (! isEmpty (CHILDREN-QUEUE))
    //          * child_task = deq (CHILDREN-QUEUE);
    //          * if (isReady (child_task)) then
    //            * assignResource (child_task);
    //            * updateResources (child_task);
    //            * enq (child_task, RUNNING-QUEUE);
    //          * else
    //            * suggestedResources (child_task);
    //          * end if
    //        * end while
    //      * end while
    //      */


    def ccfm(
      taskChildren: TaskId => Seq[TaskId]
    ) = {
      val runningQueue = scala.collection.mutable.Queue(sourceNodes: _*)
      val childQueue = scala.collection.mutable.Queue.empty[TaskId]

      def assignResource(taskId: TaskId): Unit = {}

      def updateResources(taskId: TaskId): Unit = {}

      def suggestResources(taskId: TaskId): Unit = {}

      def isReady(task: TaskId): Boolean =
        graph.get(task).diPredecessors.map(_.value).forall(runningQueue.contains)

      while (runningQueue.nonEmpty) {
        val task = runningQueue.dequeue()
        childQueue.enqueue(taskChildren(task): _*)
        while (childQueue.nonEmpty) {
          val childTask = childQueue.dequeue()
          if (isReady(childTask)) {
            assignResource(childTask)
            updateResources(childTask)
            runningQueue.enqueue(childTask)
          } else {
            suggestResources(childTask)
          }
        }
      }
    }


    //
    //    def cff(resources: ResourceId Map ResourceConfig) = {
    //      val runningQueue: Queue[TaskId] = Queue(sourceNodes: _*)
    //      def process(
    //        runningQueue: Queue[TaskId],
    //        childQueue: Queue[TaskId],
    //        completed: Seq[TaskId],
    //        resourceAssignments: Int Map Seq[TaskId]
    //      ): Seq[TaskId] = {
    //
    //        def isReady(task: TaskId): Boolean =
    //          graph.get(task).diSuccessors.map(_.value).forall(completed.contains)
    //
    //        def processChildren = {
    //          childQueue.dequeueOption.
    //        }
    //
    //        runningQueue.dequeueOption
    //
    //
    //      }
    //
    //
    //    }
    //
    //  }
    //
    //    def cff(resources: ResourceId Map ResourceConfig) = {
    //      val runningQueue: Queue[TaskId] = Queue(sourceNodes: _*)
    //      def process(
    //        runningQueue: Queue[TaskId],
    //        childQueue: Queue[TaskId],
    //        completed: Seq[TaskId],
    //        resourceAssignments: Int Map Seq[TaskId]
    //      ): Seq[TaskId] = {
    //
    //        def isReady(task: TaskId): Boolean =
    //          graph.get(task).diSuccessors.map(_.value).forall(completed.contains)
    //
    //        def processChildren = {
    //          childQueue.dequeueOption.
    //        }
    //
    //        runningQueue.dequeueOption
    //
    //
    //      }
    //
    //
    //    }
    //
    //  }


  }

//  run(
//    Task(1, 2)(
//      TaskSuccessor(2, 4),
//      TaskSuccessor(5, 1),
//      TaskSuccessor(7, 10),
//      TaskSuccessor(4, 1),
//      TaskSuccessor(3, 1)
//    ),
//    Task(2, 3)(
//      TaskSuccessor(6, 1),
//      TaskSuccessor(7, 1)
//    ),
//    Task(5, 5)(),
//    Task(7, 4)(
//      TaskSuccessor(9, 6)
//    ),
//    Task(4, 4)(
//      TaskSuccessor(8, 1)
//    ),
//    Task(3, 3)(
//      TaskSuccessor(8, 1)
//    ),
//    Task(6, 4)(
//      TaskSuccessor(9, 5)
//    ),
//    Task(7, 4)(
//      TaskSuccessor(9, 6)
//    ),
//    Task(8, 4)(
//      TaskSuccessor(9, 5)
//    ),
//    Task(9, 1)()
//  )


//  run(
//    task(1, 10),
//    task(2, 30, TaskId(1)),
//    task(3, 20, TaskId(1)),
//    task(4, 40, TaskId(2), TaskId(3)),
//    task(5, 20, TaskId(3)),
//    task(6, 10, TaskId(4), TaskId(5))
//  )

}