package com.github.dleclere.hasp.gridopt.genetics

import com.github.dleclere.hasp.gridopt.{Resource, ResourceId, Task, TaskId}

import scala.collection.SortedSet

trait ScheduleLike[Gene] {

  val name: ScheduleName

  val resourceSchedule: ResourceId Map SortedSet[(Int, Int)]

  val taskSchedule: TaskId Map ScheduledTaskLike[TaskId]

  def tasks: TaskId Map Task

  def resources: ResourceId Map Resource

  def genes: Vector[Gene]

//  def withResourceSchedule(schedule: ResourceId Map SortedSet[(Int, Int)]): ScheduleLike[Gene]

  def withScheduledTask(id: TaskId, task: ScheduledTaskLike[TaskId]): ScheduleLike[Gene]

  def withoutScheduledTask(id: TaskId): ScheduleLike[Gene]

}

object Schedule {

  def apply(
    name: ScheduleName,
    resourceSchedule: ResourceId Map SortedSet[(Int, Int)] = Map.empty.withDefaultValue(SortedSet.empty),
    taskSchedule: TaskId  Map ScheduledTaskLike[TaskId] = Map.empty,
    tasks: TaskId Map Task,
    resources: ResourceId Map Resource
  ): Schedule =
    Schedule(
      name = name,
      resourceSchedule = resourceSchedule,
      taskSchedule = taskSchedule,
      orderedSchedule = Vector.empty,
      tasks = tasks,
      resources = resources
    )


}

case class Schedule(
  name: ScheduleName,
  resourceSchedule: ResourceId Map SortedSet[(Int, Int)],
  taskSchedule: TaskId Map ScheduledTaskLike[TaskId],
  orderedSchedule: Vector[TaskId],
  tasks: TaskId Map Task,
  resources: ResourceId Map Resource
) extends ScheduleLike[ScheduledTaskLike[TaskId]] {

  def genes: Vector[ScheduledTaskLike[TaskId]] = orderedSchedule.map(taskSchedule)

//  def withResourceSchedule(schedule: ResourceId Map SortedSet[(Int, Int)]): Schedule =
//    copy(resourceSchedule = schedule)

  def withScheduledTask(id: TaskId, task: ScheduledTaskLike[TaskId]): Schedule =
    copy(
      taskSchedule =
        taskSchedule + (id -> task),
      resourceSchedule = {
        val cleared = taskSchedule
          .get(id)
          .fold(resourceSchedule)(t => resourceSchedule + (t.resource -> (resourceSchedule(t.resource) - periodRange(t))))
        cleared + (task.resource -> (cleared(task.resource) + periodRange(task)))
      },
      orderedSchedule =
        orderedSchedule.indexOf(id) match {

          case -1 =>
            orderedSchedule :+ id

          case _  =>
            orderedSchedule
        }
    )

  def withoutScheduledTask(id: TaskId): Schedule =
    copy(
      taskSchedule =
        taskSchedule - id,
      resourceSchedule =
        taskSchedule
          .get(id)
          .fold(resourceSchedule)(t => resourceSchedule + (t.resource -> (resourceSchedule(t.resource) - periodRange(t)))),
      orderedSchedule =
        orderedSchedule.filterNot(_ == id)
    )

}

case class ScheduleName(name: String)
