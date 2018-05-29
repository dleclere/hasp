package com.github.dleclere.hasp

import com.github.dleclere.hasp.Domain._

import scala.language.{higherKinds, implicitConversions}

trait GraphHelpers extends SprintLenses {

  type TaskOrStory = Either[Task, Story]

  type TaskOrStoryId = Either[TaskId, StoryId]

  type TaskOrStoryIdConv[I] = I => TaskOrStoryId

  implicit def leftTask(id: TaskId): TaskOrStoryId = Left(id)

  implicit def rightStory(id: StoryId): TaskOrStoryId = Right(id)

  def workIdExtractor(work: Work[_]): TaskOrStoryId =
    work match {

      case t: Task =>
        Left(t.id)

      case s: Story =>
        Right(s.id)

    }

  def workIdExtractor(work: TaskOrStory): TaskOrStoryId =
    work.fold(_.id, _.id)

  def addPoints(a: StoryPoints, b: StoryPoints): StoryPoints =
    StoryPoints(a.points + b.points)

  def sumPoints(points: Iterable[StoryPoints]): StoryPoints =
    points.foldLeft(StoryPoints(0))(addPoints)

  def completedPoints(sprint: Sprint): StoryPoints =
    sumPoints {
      sprint
        .completed
        .map(_.actualPoints)
    }

  def velocity(project: ScrumProject): StoryPoints =
    sumPoints {
      project
        .previousSprints
        .map(completedPoints)
    }

}
