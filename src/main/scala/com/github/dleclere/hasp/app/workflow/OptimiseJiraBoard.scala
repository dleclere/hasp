package com.github.dleclere.hasp.app.workflow

import com.github.dleclere.hasp.app.DemoBootstrap
import com.github.dleclere.hasp.app.config._
import com.github.dleclere.hasp.app.converters.JiraIssues2GridTasks
import com.github.dleclere.hasp.app.models
import com.github.dleclere.hasp.app.models.{AppUser, JiraAppWorkspaceName}
import com.github.dleclere.hasp.jira.JiraApi.fetchBacklogIssues
import com.github.dleclere.hasp.jira._
import com.github.dleclere.hasp.jira.models.{JiraBoardId, JiraIssue, JiraIssueId, JiraProjectKey, JiraUserName}
import cats.effect._
import com.github.dleclere.hasp.utils._
import cats.instances.list._
import cats.syntax.parallel._
import com.github.dleclere.hasp.app.stores._
import com.github.dleclere.hasp.app.sync.{syncJiraIssueAssignees, syncJiraIssueRanks}
import com.github.dleclere.hasp.genetics
import com.github.dleclere.hasp.genetics.BaseImps._
import cats.syntax.all._
import com.github.dleclere.hasp.gridopt.ScheduleUtils._
import com.github.dleclere.hasp.gridopt.{ResourceId, TaskId}
import com.github.dleclere.hasp.gridopt.genetics.{CrossoverScheduledTask, GeneticContext, MutateScheduledTask, ScheduleLike, ScheduleName, ScheduledTask, ScheduledTaskLike}

import scala.collection.parallel.immutable.ParVector
import scala.util.Try

object OptimiseJiraBoard {

  def optimise(workspaceName: JiraAppWorkspaceName)(implicit config: AppRepoConfig with HttpClientConfig) = {//: IO[Vector[JiraIssueId]] = {
    import config._
    import config.appRepo._
    import config.scheduleRepo._
    val scheduleName = ScheduleName(workspaceName.name)

    def run(ctx: GeneticContext): Either[Throwable, ScheduleLike[ScheduledTaskLike[TaskId]]] = {
      import ctx._
      eitherError(
        genetics.run[Vector, Vector, ParVector, ScheduleLike, ScheduledTaskLike[TaskId]](
          Vector(MutateScheduledTask),
          Vector(CrossoverScheduledTask),
          initialPopulationSize = 2,
          populationLimit = 2000,
          cullRate = 0.7f,
          cullPopulationThreshold = 40,
          maxGenerations = 10,
          crossoverRate = 0.7,
          mutationRate = 0.7
        ).headOption
      )(new Error(s"Failed to generate schedule ${ctx.scheduleName}"))
    }
    // TODO make this safe!
    def res2JiraUser(resource: ResourceId, users: Seq[AppUser]): JiraUserName =
      users.find(_.resource == resource).map(_.jiraName).get

    def tsk2IssueId(task: ScheduledTaskLike[TaskId]): JiraIssueId =
      JiraIssueId(task.id.id)

    jiraWSCfg(workspaceName).flatMap { appConfig =>
      implicit val appCfg = appConfig
      implicit val apiCfg = appConfig.apiConfig

      def getBacklog: IO[Vector[JiraIssue]] =
        streamResults(fetchBacklogIssues())
          .compile.toVector.map(
          _.filter(_.rank.nonEmpty)
            .sortBy(_.rank.get)
        )

      for {
        _ <- DemoBootstrap.run()
        backlog <- getBacklog
        tasks = JiraIssues2GridTasks(backlog)
        users <- getAllUsers(workspaceName).compile.toVector
        resources <- parIos(users.map(userResource)).map(mkIdMap(_.id))
        latestFinish <- IO.fromEither(estWorstFinish(tasks.values, resources.values))
        geneticContext = new GeneticContext(scheduleName, tasks, resources, latestFinish)
        fittest <- IO.fromEither(run(geneticContext))
        taskOrder = fittest.genes.sortBy(_.start.num)
        issueOrder = taskOrder.map(_.id.id).map(JiraIssueId)
        _ <- syncJiraIssueRanks(issueOrder)
        allocs <- IO.fromEither(Try(mkIdMap(tsk2IssueId)(taskOrder).mapValues(s => res2JiraUser(s.resource, users))).toEither)
        _ <- syncJiraIssueAssignees(allocs)
      } yield issueOrder.flatMap(id => backlog.find(_.id == id).map(_.key).map(_ -> allocs(id)))
    }
  }

}
