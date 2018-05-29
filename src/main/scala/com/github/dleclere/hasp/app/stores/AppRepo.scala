package com.github.dleclere.hasp.app.stores

import com.github.dleclere.hasp.app.models._
import cats.effect.IO

trait AppRepo {

  def getUser(userId: AppEntityId[AppUserId]): RepoGetResult[AppUser]

  def setUser(userId: AppEntityId[AppUserId], user: AppUser): RepoSetResult

  def getAllUsers(workspaceName: JiraAppWorkspaceName): RepoQueryResult[AppUser]

  def getJiraWorkspace(workspaceName: JiraAppWorkspaceName): RepoGetResult[JiraAppWorkspace]

  def setJiraWorkspace(workspaceName: JiraAppWorkspaceName, workspace: JiraAppWorkspace): RepoSetResult

}
