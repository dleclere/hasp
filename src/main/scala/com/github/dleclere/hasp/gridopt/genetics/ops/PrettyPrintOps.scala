package com.github.dleclere.hasp.gridopt.genetics.ops

import com.github.dleclere.hasp.gridopt.genetics.{ScheduleLike, ScheduledTask, ScheduledTaskLike, periodRange}
import com.github.dleclere.hasp.gridopt.{ResourceId, TaskId}
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge

import scala.collection.{BitSet, SortedSet}

object PrettyPrintOps {


  def tpl2BitSet(tpl: (Int, Int)): BitSet =
    BitSet(tpl._1 to tpl._2: _*)

  //def contains(s: SortedSet[(Int, Int)]):

  def prettyPrint(graph: Graph[TaskId, DiEdge], schedule: ScheduleLike[ScheduledTaskLike[TaskId]]): Unit = {
    case class Row(tasks: Vector[Char], resources: Vector[Seq[Int]]) {

      def isEmpty: Boolean = tasks.forall(_ == ' ') && resources.forall(_.isEmpty)

    }

    def prettyPrintTaskSchedule(graph: Graph[TaskId, DiEdge], schedule: ScheduleLike[ScheduledTaskLike[TaskId]]): Unit = {
      println("Task Schedule")
      val sortedTasks = schedule.taskSchedule.values.toSeq.sortBy(_.start.num)
      val taskIds = sortedTasks.map(_.id)
      val lastFinish = sortedTasks.maxBy(_.end.num).end.num
      val resourceIds = (sortedTasks.map(_.resource) ++ schedule.resourceSchedule.keys.toSeq).distinct
      val rows = Vector.fill[Row](lastFinish)(Row(Vector.fill[Char](taskIds.length)(' '), Vector.fill[Seq[Int]](resourceIds.size)(Seq.empty)))
      print(f"${'P'}%5s")
      taskIds.map(_.id).foreach(c => print(f"$c%5d"))

      print(f"${' '}%15s")
      resourceIds.map(_.id).foreach(c => print(f"$c%15d|"))
      print('\n')
      sortedTasks
        .zipWithIndex
        .map { case (s, index) =>
          (index, s, periodRange(s.start, s.end))
        }.foldLeft(rows) { case (rs, (taskIndex, ScheduledTask(taskId, resourceId, start, _), range)) =>
        tpl2BitSet(range).foldLeft(rs) { (rs, index) =>
          val row = rs(index)
          val rIndex = resourceIds.indexOf(resourceId)
          rs.updated(index, row.copy(
            tasks = row.tasks.updated(taskIndex, '#'),
            resources = row.resources.updated(rIndex, row.resources(rIndex) :+ taskId.id)
          ))
        }
      }
        .zipWithIndex
        .filterNot(_._1.isEmpty)
        .foreach { case (Row(tasks, resources), index) =>
          print(f"${index}%5d")
          tasks.foreach(c => print(f"$c%5s"))
          print(f"${' '}%15s")
          resources
            .map(r => r.mkString(","))
            .foreach(c => print(f"$c%15s"))
          print('\n')
        }
      print('\n')

    }

    def prettyPrintResourceSchedule(): Unit = {
      println("Resource Schedule")
      val sch = schedule.resourceSchedule
      val rids = sch.keys.map(_.id).toSeq.sorted
      val total = sch.values.flatten
      val start = total.map(_._1).min
      val end = total.map(_._2).max
      print(f"${"P"}%5s")
      rids.foreach(r => print(f"$r%5s"))
      (start to end)
        .foreach { p =>
          print(f"$p%5s")
          rids.foreach { id =>
            val c = if (sch(ResourceId(id)).flatMap(tpl2BitSet).contains(p)) '#' else ' '
            print(f"$c%5s")
          }
          print('\n')
        }
      print('\n')
    }

    def prettyPrintScheduleTasks(): Unit = {
      printf(f"${"deps"}%8s${"task"}%7s${"rsrc"}%7s${"start"}%7s${"end"}%7s%n")
      schedule.taskSchedule.values
        .toSeq
        .sortBy(_.start.num)
        .foreach { case ScheduledTask(taskId, resourceId, start, end) =>
          val dependsOn = graph.get(taskId).diPredecessors.map(_.value).map(_.id)
          printf(f"${dependsOn.mkString(",")}%8s${taskId.id}%7s${resourceId.id}%7s${start.num}%7s${end.num}%7s%n")
        }
    }


    prettyPrintTaskSchedule(graph, schedule)

    prettyPrintResourceSchedule()

    prettyPrintScheduleTasks()
  }

}
