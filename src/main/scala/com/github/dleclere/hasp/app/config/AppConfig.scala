package com.github.dleclere.hasp.app.config

import com.github.dleclere.hasp.app.models.{JiraAppWorkspace, JiraAppWorkspaceName}
import com.github.dleclere.hasp.app.stores.{AppRepo, ScheduleRepo}
import com.github.dleclere.hasp.jira._
import cats.effect.IO
import org.http4s.{BasicCredentials, Uri}
import org.http4s.client.Client
import org.http4s.headers.Authorization

trait CoreAppConfig {

}

trait HttpClientConfig {

  val client: Client[IO]


}

trait AppRepoConfig {

  val appRepo: AppRepo

  val scheduleRepo: ScheduleRepo

}


trait JiraAppWorkspaceConfig {

  val workspace: JiraAppWorkspace

  val apiConfig: ApiConfig

}