package com.github.dleclere.hasp.gridopt.genetics

import com.github.dleclere.hasp.gridopt.{Period, ResourceId, TaskId}


trait ScheduledTaskLike[Id] {
  val id: Id
  val resource: ResourceId
  val start: Period
  val end: Period
}

case class ScheduledTask(id: TaskId, resource: ResourceId, start: Period, end: Period) extends ScheduledTaskLike[TaskId]
