package com.github.dleclere.hasp.app.stores.db

import com.github.dleclere.hasp.app.models.{JiraAppWorkspace, JiraAppWorkspaceName}
import com.github.dleclere.hasp.app.{models, stores}
import com.github.dleclere.hasp.app.stores._
import com.github.dleclere.hasp.jira.models._
import com.github.dleclere.hasp.utils._
import doobie._
import doobie.implicits._
import cats._
import cats.effect._
import cats.implicits._
import org.http4s.Uri

import scala.language.implicitConversions

class DoobieAppRepo(transactor: Transactor.Aux[IO, Unit]) extends AppRepo {

  override def getUser(userId: stores.AppEntityId[models.AppUserId]): RepoGetResult[models.AppUser] =
    throwsFetchErr(userId) {
      sql"""SELECT id, jiraName, resource FROM AppUser WHERE workspace = ${userId.workspace.name} AND id = ${userId.entity.id}"""
        .query[models.AppUser].option
    }

  override def setUser(userId: stores.AppEntityId[models.AppUserId], user: models.AppUser): RepoSetResult =
    sql"""
         |INSERT INTO AppUser (workspace, id, jiraName, resource)
         |  VALUES (${userId.workspace.name}, ${userId.entity.id}, ${user.jiraName}, ${user.resource.id})
         |  ON CONFLICT (workspace, id) DO UPDATE
         |  SET jiraName=EXCLUDED.jiraName, resource=EXCLUDED.resource
       """.stripMargin
        .update
        .run

  override def getAllUsers(workspaceName: models.JiraAppWorkspaceName): RepoQueryResult[models.AppUser] =
    sql"""SELECT id, jiraName, resource FROM AppUser WHERE workspace = ${workspaceName.name}"""
      .query[models.AppUser]
      .stream
      .transact(transactor)


  override def getJiraWorkspace(workspaceName: models.JiraAppWorkspaceName): RepoGetResult[models.JiraAppWorkspace] =
    throwsFetchErr(AppEntityId(workspaceName, workspaceName)) {
      sql"""
           |SELECT name, apiUsername, apiPassword, root, board, project FROM JiraAppWorkspace
           |  WHERE name = ${workspaceName.name}
       """.stripMargin
        .query[(String, String, String, String, Int, String)]
        .map {

          case (name, user, pass, root, board, project) =>
            JiraAppWorkspace(
              name = JiraAppWorkspaceName(name),
              apiUsername = user,
              apiPassword = pass,
              root = Uri.unsafeFromString(root),
              board = JiraBoardId(board),
              project = JiraProjectKey(project)
            )

        }
        .option
    }

  override def setJiraWorkspace(workspaceName: models.JiraAppWorkspaceName, workspace: models.JiraAppWorkspace): RepoSetResult = {
    import workspace._
    sql"""
         |INSERT INTO JiraAppWorkspace (name, apiUsername, apiPassword, root, board, project)
         |  VALUES (${workspace.name}, $apiUsername, $apiPassword, ${root.toString()}, ${board.id}, ${project.key})
         |  ON CONFLICT (name) DO UPDATE
         |  SET apiUsername=EXCLUDED.apiUsername, apiPassword=EXCLUDED.apiPassword, root=EXCLUDED.root, board=EXCLUDED.board, project=EXCLUDED.project
       """.stripMargin
      .update
      .run
  }

  private implicit def cio2io[T](cio: ConnectionIO[T]): IO[T] =
    cio.transact(transactor)

  private implicit def cio2ioUnit(cio: ConnectionIO[Int]): IO[Unit] =
    cio.map(_ => ()).transact(transactor)

  def throwsFetchErr[T, Id](id: AppEntityId[Id])(fetch: IO[Option[T]]): IO[T] =
    fetch
      .map(eitherErrorv2[T, RepoFetchError[Id] with Throwable](_, EntityDoesNotExist(id)))
      .flatMap(IO.fromEither[T](_))

}
