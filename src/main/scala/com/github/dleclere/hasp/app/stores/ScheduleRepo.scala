package com.github.dleclere.hasp.app.stores

import com.github.dleclere.hasp.app.config.JiraAppWorkspaceConfig
import com.github.dleclere.hasp.app.models.JiraAppWorkspaceName
import com.github.dleclere.hasp.gridopt.genetics.ScheduleName
import com.github.dleclere.hasp.gridopt.{ExecRatePerPeriod, Resource, ResourceId, ScheduledTask, Task, TaskId, TaskSuccessor, Work}
import cats.effect.IO

trait ScheduleRepo {

  def createResource(workspace: JiraAppWorkspaceName, execRatePerPeriod: ExecRatePerPeriod): RepoCreateResult[Resource]

  def getResource(id: AppEntityId[ResourceId]): RepoGetResult[Resource]

  def setResource(id: AppEntityId[ResourceId], resource: Resource): RepoSetResult

  def createTask(work: Seq[Work], successors: Seq[TaskSuccessor], predecessors: Seq[TaskId]): RepoCreateResult[Task]

  def getTask(id: AppEntityId[TaskId]): RepoGetResult[Task]

  def setTask(id: AppEntityId[TaskId], task: Task): RepoSetResult

  def getScheduledTask(id: AppEntityId[TaskId]): RepoGetResult[ScheduledTask]

  def setScheduledTask(id: AppEntityId[TaskId], ScheduledTask: ScheduledTask): RepoSetResult

  def getAllScheduledTasks(workspaceName: JiraAppWorkspaceName): RepoQueryResult[ScheduledTask]
  
}
