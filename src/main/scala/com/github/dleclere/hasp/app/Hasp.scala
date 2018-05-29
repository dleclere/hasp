package com.github.dleclere.hasp.app

import com.github.dleclere.hasp.app.config.{AppRepoConfig, HttpClientConfig}
import com.github.dleclere.hasp.app.models.{JiraAppWorkspace, JiraAppWorkspaceName}
import com.github.dleclere.hasp.app.service.Service
import com.github.dleclere.hasp.app.stores.{AppRepo, ScheduleRepo, db}
import com.github.dleclere.hasp.app.stores.db.connections.{ConnectionManager, JDBCManager}
import com.github.dleclere.hasp.app.workflow.OptimiseJiraBoard
import com.github.dleclere.hasp.jira.models.{JiraBoardId, JiraProjectKey}
import cats.effect.IO
import fs2.StreamApp.ExitCode
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.client.blaze.Http1Client
import org.http4s.server.blaze._

import scala.concurrent.ExecutionContext.Implicits.global

object Hasp extends App {

  val connectionConfig = ConnectionManager.Config(
    driver = "org.postgresql.Driver",
    uri = "jdbc:postgresql:thesis",
    user = "hasp",
    pass = ""
  )
  val transactor = new JDBCManager(connectionConfig).transactor


  val workspace = JiraAppWorkspace(
    name = JiraAppWorkspaceName("DemoWorkspace"),
    apiUsername = "hasp",
    apiPassword = "jira",
    root = Uri.uri("http://localhost:8080"),
    board = JiraBoardId(1),
    project = JiraProjectKey("LP")
  )

  def mkAppConfig(repoConfig: AppRepoConfig, httpClient: Client[IO]): AppRepoConfig with HttpClientConfig = new AppRepoConfig with HttpClientConfig {
    override val appRepo: AppRepo = repoConfig.appRepo
    override val scheduleRepo: ScheduleRepo = repoConfig.scheduleRepo
    override val client: Client[IO] = httpClient
  }

  val startup =
    for {
      dbConfig <- db.setup(transactor)
      client <- Http1Client[IO]()
      appConfig = mkAppConfig(dbConfig, client)
      _ <- dbConfig.appRepo.setJiraWorkspace(workspace.name, workspace)
      //schedule <- OptimiseJiraBoard.optimise(workspace.name)(appConfig)
    } yield appConfig

  val config = startup.unsafeRunSync()

  val server = BlazeBuilder[IO].bindHttp(9898, "localhost")
    .mountService(Service.routes(config), "/")
    .serve
    .compile.drain

  println(s"RESULT ${server.unsafeRunSync()}")

}
