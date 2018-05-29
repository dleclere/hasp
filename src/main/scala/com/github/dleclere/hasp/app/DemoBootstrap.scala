package com.github.dleclere.hasp.app

import com.github.dleclere.hasp.app.config.{AppRepoConfig, JiraAppWorkspaceConfig}
import com.github.dleclere.hasp.app.models.{AppUser, AppUserId}
import com.github.dleclere.hasp.gridopt.{ExecRatePerPeriod, WorkAmount, WorkTypeTag}
import com.github.dleclere.hasp.jira.models.JiraUserName
import cats.effect.IO
import com.github.dleclere.hasp.utils._

import scala.util.Random

object DemoBootstrap {

  def run()(implicit config: JiraAppWorkspaceConfig with AppRepoConfig): IO[Unit] = {
    import config.apiConfig._
    import com.github.dleclere.hasp.jira.JiraApi._
    import config.scheduleRepo._
    import config.appRepo._
    def work: ExecRatePerPeriod =
      ExecRatePerPeriod {
        Seq("general", "frontend", "backend", "design")
          .map(WorkTypeTag(_))
          .map(_ -> WorkAmount(Random.nextInt(10) + 1))
          .toMap
      }


    for {
      localUsers <- config.appRepo.getAllUsers(config.workspace.name).compile.toVector
      remoteUsers <- fetchUsers(config.apiConfig)
      newUsers = remoteUsers.map(_.id.name) diff localUsers.map(_.id.id)
      newResources <- parIos(newUsers.map(u => createResource(config.workspace.name, work).map(_.id).map(u -> _)))
      _ <- parIos(newResources.map { case (user, resource) => setUser(AppUserId(user), AppUser(AppUserId(user), JiraUserName(user), resource)) })

    } yield Unit
  }

}
