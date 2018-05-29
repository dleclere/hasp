package com.github.dleclere.hasp.app.stores.db


import com.github.dleclere.hasp.app.config.JiraAppWorkspaceConfig
import com.github.dleclere.hasp.app.models
import com.github.dleclere.hasp.app.models.JiraAppWorkspaceName
import com.github.dleclere.hasp.app.stores._
import com.github.dleclere.hasp.gridopt.{ExecRatePerPeriod, Resource, ResourceId, ScheduledTask, Task, TaskId, TaskSuccessor, Work}
import com.github.dleclere.hasp.jira.models._
import com.github.dleclere.hasp.utils._
import doobie._
import doobie.implicits._
import cats._
import cats.effect._
import cats.implicits._
import io.circe._
import io.circe.jawn._
import io.circe.syntax._

import scala.reflect.runtime.universe._
import org.postgresql.util.PGobject

import scala.language.implicitConversions

class DoobieScheduleRepo(transactor: Transactor.Aux[IO, Unit]) extends ScheduleRepo {

  implicit val JsonMeta: Meta[Json] =
    Meta.other[PGobject]("json").xmap[Json](
      a => parse(a.getValue).leftMap[Json](e => throw e).merge, // failure raises an exception
      a => {
        val o = new PGobject
        o.setType("json")
        o.setValue(a.noSpaces)
        o
      }
    )

  def codecMeta[A : Encoder : Decoder : TypeTag]: Meta[A] =
    Meta[Json].xmap[A](
      _.as[A].fold[A](throw _, identity),
      _.asJson
    )

  implicit val capacityMeta: doobie.Meta[ExecRatePerPeriod] = codecMeta[ExecRatePerPeriod]


  override def createResource(workspace: JiraAppWorkspaceName, capacity: ExecRatePerPeriod): RepoCreateResult[Resource] =
    sql"""
         |INSERT INTO Resource (workspace, capacity)
         |  VALUES (${workspace.name}, $capacity)
       """.stripMargin
      .update
      .withUniqueGeneratedKeys[(String, ExecRatePerPeriod)]("id", "capacity")
      .map { case tpl@(str1, _capacity) =>
        Resource(ResourceId(str1.toInt), _capacity)
      }


  override def getResource(id: AppEntityId[ResourceId]): RepoGetResult[Resource] =
    throwsFetchErr(id) {
      sql"""SELECT id, capacity FROM Resource WHERE workspace = ${id.workspace.name} AND id = ${id.entity.id}"""
        .query[Resource].option
    }

  override def setResource(id: AppEntityId[ResourceId], resource: Resource): RepoSetResult =
    sql"""
         |UPDATE Resource SET capacity = ${resource.execRatePerPeriod}
         |  WHERE workspace=${id.workspace.name} AND id = ${id._2.id}
       """.stripMargin
      .update
      .run




  override def createTask(work: Seq[Work], successors: Seq[TaskSuccessor], predecessors: Seq[TaskId]): RepoCreateResult[Task] = ???

  override def getTask(id: AppEntityId[TaskId]): RepoGetResult[Task] = ???

  override def setTask(id: AppEntityId[TaskId], task: Task): RepoSetResult = ???

  override def getScheduledTask(id: AppEntityId[TaskId]): RepoGetResult[ScheduledTask] = ???

  override def setScheduledTask(id: AppEntityId[TaskId], ScheduledTask: ScheduledTask): RepoSetResult = ???

  override def getAllScheduledTasks(workspaceName: models.JiraAppWorkspaceName): RepoQueryResult[ScheduledTask] = ???


  private implicit def cio2io[T](cio: ConnectionIO[T]): IO[T] =
    cio.transact(transactor)

  private implicit def cio2ioUnit(cio: ConnectionIO[Int]): IO[Unit] =
    cio.map(_ => ()).transact(transactor)

  def throwsFetchErr[T, Id](id: AppEntityId[Id])(fetch: IO[Option[T]]): IO[T] =
    fetch
      .map(eitherErrorv2[T, RepoFetchError[Id] with Throwable](_, EntityDoesNotExist(id)))
      .flatMap(IO.fromEither[T](_))





}
