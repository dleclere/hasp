package com.github.dleclere.hasp.app

import com.github.dleclere.hasp.app.config.{AppRepoConfig, HttpClientConfig, JiraAppWorkspaceConfig}
import com.github.dleclere.hasp.app.models.{AppUser, AppUserId, JiraAppWorkspace, JiraAppWorkspaceName}
import com.github.dleclere.hasp.gridopt.Resource
import com.github.dleclere.hasp.jira._
import cats.effect.IO
import org.http4s.BasicCredentials
import org.http4s.client.Client
import org.http4s.headers.Authorization
import com.github.dleclere.hasp.app._
import com.github.dleclere.hasp.app.stores.{AppRepo, ScheduleRepo}

package object workflow {

  def jiraWSCfg(
    workspaceName: JiraAppWorkspaceName
  )(implicit config: AppRepoConfig with HttpClientConfig): IO[AppRepoConfig with HttpClientConfig with JiraAppWorkspaceConfig] =
    for {
      workspaceModel <- config.appRepo.getJiraWorkspace(workspaceName)
      init = jiraInitialApiConfig(config.client, workspaceModel)
      api <- ApiConfig(init)
    } yield {
      new AppRepoConfig with HttpClientConfig with JiraAppWorkspaceConfig {

        val client: Client[IO] = config.client
        val appRepo: AppRepo = config.appRepo
        val scheduleRepo: ScheduleRepo = config.scheduleRepo
        val workspace: JiraAppWorkspace = workspaceModel
        val apiConfig: ApiConfig = api
      }
    }

  private def jiraInitialApiConfig(client: Client[IO], workspace: JiraAppWorkspace): BaseApiConfig with BoardAwareApiConfig with ProjectAwareApiConfig =
    InitialApiConfig(
      client = client,
      authorization = Authorization(BasicCredentials(workspace.apiUsername, workspace.apiPassword)),
      root = workspace.root,
      board = workspace.board,
      project = workspace.project
    )

  def userResource(user: AppUser)(implicit config: JiraAppWorkspaceConfig with AppRepoConfig): IO[Resource] =
    config.scheduleRepo.getResource(user.resource)

}
