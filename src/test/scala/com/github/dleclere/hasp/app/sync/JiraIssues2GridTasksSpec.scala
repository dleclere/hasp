package com.github.dleclere.hasp.app.sync

import com.github.dleclere.hasp.app.converters.JiraIssues2GridTasks
import com.github.dleclere.hasp.genetics
import com.github.dleclere.hasp.genetics.BaseImps._
import com.github.dleclere.hasp.gridopt.genetics.{MutateScheduledTask, ScheduleLike, ScheduledTask, _}
import com.github.dleclere.hasp.gridopt.{ExecRatePerPeriod, Period, Resource, ResourceId, TaskId, WorkAmount, WorkTypeTag}
import com.github.dleclere.hasp.jira.{JiraApi, _}
import com.github.dleclere.hasp.jira.models._
import cats.effect.IO
import org.http4s.client.blaze._
import org.http4s.headers.Authorization
import org.http4s.{BasicCredentials, Uri}
import org.scalatest.{FlatSpec, Matchers}
import com.github.dleclere.hasp.app.sync._

import scala.collection.parallel.immutable.ParVector
import scala.util.Random

class JiraIssues2GridTasksSpec extends FlatSpec with Matchers {
  import com.github.dleclere.hasp.JiraServerConfig._


  import JiraApi._


  def getBacklog: Vector[JiraIssue] =
    streamResults(fetchBacklogIssues(0, 50))
      .compile.toVector.unsafeRunSync()
      .filter(_.rank.nonEmpty)
      .sortBy(_.rank.get)

  "A stream for the backlog issues" should "task map" in {
    val allIssues =
      streamResults(fetchAllBoardIssues(0, 50))
        .compile.toVector.unsafeRunSync()
    val tasks = JiraIssues2GridTasks(allIssues)
    tasks should not be (empty)
  }

  "A stream for the backlog issues" should "work for everything!" in {

    val backlog = {
      val startBacklog = getBacklog

      val shuffledBacklog =
        Random
          .shuffle(startBacklog)
          .map(_.id)
      val syncResult = syncJiraIssueRanks {
        shuffledBacklog
      }.unsafeRunSync()
      syncResult should be (true)
      val nextBacklog = getBacklog
      Thread.sleep(5000)
      nextBacklog should not be (startBacklog)
      nextBacklog.map(_.id) should be (shuffledBacklog)
      nextBacklog
    }

    println(s"BACKLOG ISSUES: ${backlog.map(_.id.id).sorted.mkString(", ")}")

    val tasks = JiraIssues2GridTasks(backlog)

    println(s"JiraIssues2GridTasks: ${tasks.keys.toSeq.map(_.id).sorted.mkString(", ")}")

    val taskIds = tasks.keys.toSeq

    val taskIdInts = taskIds.map(_.id)

    backlog.map(_.id.id).forall(taskIdInts.contains) should be (true)

    val successorIds = tasks.values.flatMap(_.successors).map(_.taskId).toSeq

    val predIds = tasks.values.flatMap(_.predecessors).toSeq

    assert(successorIds.forall(taskIds.contains))

    assert(predIds.forall(taskIds.contains))

    def work: ExecRatePerPeriod =
      ExecRatePerPeriod {
        (tasks
          .values
          .flatMap(_.work.map(_.tag))
          .toSeq
          .distinct :+ WorkTypeTag("general"))
          .map(_ -> WorkAmount(Random.nextInt(10) + 1))
          .toMap
      }

    println("Generating context")

    val users = fetchUsers.unsafeRunSync()

    val (user2Resource, resources) = {
      val intermediate =
        users
          .map(_.id)
          .distinct
          .zipWithIndex
          .map { case (id, index) =>
            ResourceId(index) -> (id, Resource(ResourceId(index), work))
          }
      (intermediate.map(kv => kv._1 -> kv._2._1).toMap, intermediate.map(kv => kv._1 -> kv._2._2).toMap)
    }
    users should not be (empty)
    println(s"User map ${user2Resource.mkString(", ")}")
    println(s"Resources: ${resources.values.mkString(", ")}")

    val minResourceCapacity = resources.values.flatMap(_.execRatePerPeriod.capacity.values.map(_.value)).sum.toFloat

    val latestFinish = Period {
      (taskIds.length.toFloat * (tasks.values.flatMap(_.work.map(_.amount.value)).sum.toFloat / minResourceCapacity).round)
        .toInt
    }
    println("latest finish: " + latestFinish)
    val testContext = new GeneticContext(
      scheduleName = ScheduleName("test"),
      tasks,
      resources = resources,
      latestFinish = latestFinish
    )

    import testContext._
    val result = genetics.run[Vector, Vector, ParVector, ScheduleLike, ScheduledTaskLike[TaskId]](
      Vector(MutateScheduledTask),
      Vector(CrossoverScheduledTask),
      initialPopulationSize = 2,
      populationLimit = 200,
      cullRate = 0.6f,
      cullPopulationThreshold = 40,
      maxGenerations = 900,
      crossoverRate = 0.7,
      mutationRate = 0.7
    )
    result.seq.take(4)
      .foreach { result =>
        println("\t\t" + result.genes.maxBy(_.end.num).end)
      }
    result should not be (empty)
    val optimalSchedule = result.head
    println(s"Opt Schedule \n${optimalSchedule.genes.map("\t" + _).mkString("\n")}")
    println(s"Least Opt Schedule \n${result.last.genes.map("\t" + _).mkString("\n")}")

    val newTaskOrder =
      optimalSchedule
        .genes
        .sortBy(_.start.num)
        .map(_.id.id)
        .map(JiraIssueId)
    syncJiraIssueRanks(newTaskOrder).unsafeRunSync()
    val nextBacklog = getBacklog
    //nextBacklog should not be (backlog)
    nextBacklog
      .map(_.id)
      .filter(newTaskOrder.contains) should be (newTaskOrder)

    val allocations =
      optimalSchedule
        .genes
        .map(st => JiraIssueId(st.id.id) -> user2Resource(st.resource))
        .toMap
    syncJiraIssueAssignees(allocations).unsafeRunSync()
    Thread.sleep(3000)
    val finalBacklog = getBacklog
    val p1 = finalBacklog
    val p2 = p1.map(issue => issue.id -> issue.assignee)
    val p3 = p2.filter(issue => allocations.contains(issue._1))
    val p4 = p3.sortBy(_._1.id)
    val expected = allocations.toSeq.sortBy(_._1.id).map {
      case (a1, b) =>
        (a1, Some(b))
    }
    println(s"ALl ${allocations.map(_._1).mkString(", ")}")
    println(s"Allocations: ${allocations.map(_._2)}")
    p4 should be (expected)
  }

}
