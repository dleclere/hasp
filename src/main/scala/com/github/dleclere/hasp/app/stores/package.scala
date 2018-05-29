package com.github.dleclere.hasp.app

import com.github.dleclere.hasp.app.config.JiraAppWorkspaceConfig
import com.github.dleclere.hasp.app.models.JiraAppWorkspaceName
import com.github.dleclere.hasp.app.stores._
import com.github.dleclere.hasp.jira.models._
import com.github.dleclere.hasp.app.models._
import cats.effect.IO

package object stores {

  implicit def id2aei[T](id: T)(implicit config: JiraAppWorkspaceConfig): AppEntityId[T] =
    AppEntityId[T](config.workspace.name, id)

  case class AppEntityId[Id](
    workspace: JiraAppWorkspaceName,
    entity: Id
  ) extends Product2[JiraAppWorkspaceName, Id] {

    override def _1: JiraAppWorkspaceName = workspace

    override def _2: Id = entity
  }

  type RepoSetResult = IO[Unit]

  type RepoGetResult[Model] = IO[Model]

  type RepoCreateResult[Model] = IO[Model]

  type RepoQueryResult[Result] = fs2.Stream[IO, Result]

  type RepoDeleteResult = IO[Unit]

  sealed trait RepoError {
    self: Throwable =>
  }

  sealed trait RepoFetchError[Id] extends RepoError {
    self: Throwable =>

    val id: AppEntityId[Id]

  }

  case class EntityDoesNotExist[Id](id: AppEntityId[Id]) extends Throwable with RepoFetchError[Id]

  case class UnableToRetrieveEntity[Id](id: AppEntityId[Id], parent: Throwable) extends Throwable(parent) with RepoFetchError[Id]

  case class UnableToUpsertEntity[Id](id: AppEntityId[Id], parent: Throwable) extends Throwable(parent) with RepoError

  case class UnableToCreateEntity(parent: Throwable) extends Throwable(parent) with RepoError

  case class UnableToExecuteQuery(parent: Throwable) extends Throwable(parent) with RepoError

  case class UnableToDeleteEntity[Id](id: AppEntityId[Id], parent: Throwable) extends Throwable

}
