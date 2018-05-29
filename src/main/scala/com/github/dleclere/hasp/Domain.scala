package com.github.dleclere.hasp

import com.github.dleclere.hasp.Domain.KanbanPeriod.StoryAllocationConfig
import com.github.dleclere.hasp.backlog.Backlog
import monocle.{Lens, Traversal}
import monocle.macros.{GenLens, Lenses}
import monocle.function.all._
import monocle.macros.syntax.lens._
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

import scala.annotation.tailrec

object Domain {

  case class TeamMemberId(id: String)// extends AnyVal

  object TeamMemberConfig {

    def effectivePoints(config: TeamMemberConfig, nominal: StoryPoints, workTypeMap: WorkTypeTag Map Double): Option[StoryPoints] =
      workTypeMap
        .foldLeft(Option(0d)) {
          case (memo, (tag, weight)) =>
            memo flatMap { m =>
              config.velocityMultiplierByTag
                .get(tag)
                .map(weight / _)
                .map(m + _)
            }
        }
        .map(nominal.points.toDouble * _)
        .map(_.toInt)
        .map(Math.max(1, _))
        .map(StoryPoints)

//    def effectivePoints(config: TeamMemberConfig, nominal: StoryPoints, workType: WorkTypeTag): Option[StoryPoints] =
//      config.velocityMultiplierByTag
//        .get(workType)
//        .map(1 - _)
//        .map(Math.min(_, 1))
//        .map(nominal.points * _)
//        .map(Math.max(1, _).toInt)
//        .map(StoryPoints)

  }

  case class TeamMemberConfig(name: TeamMemberId, velocityMultiplierByTag: WorkTypeTag Map Double)

  sealed trait Work[Id] {

    val id: Id

    val dependents: Seq[Id]

    val precedents: Seq[Id]

  }

  case class StoryPoints(points: Int)// extends AnyVal

  sealed trait SprintWorkState {

    val workId: TaskOrStoryId

    val assignee: TeamMemberId

  }

  case class CompletedWork(workId: TaskOrStoryId, assignee: TeamMemberId, actualPoints: StoryPoints) extends SprintWorkState

  case class InProgressWork(workId: TaskOrStoryId, assignee: TeamMemberId, consumedPoints: StoryPoints) extends SprintWorkState

  case class StoryId(id: String)// extends AnyVal

  case class Story(
    id: StoryId,
    points: StoryPoints,
    workTypeFractions: WorkTypeTag Map Double,
    dependents: Seq[StoryId] = Seq.empty,
    precedents: Seq[StoryId] = Seq.empty,
    children: Seq[TaskId] = Seq.empty
  ) extends Work[StoryId]

  case class TaskId(id: String)// extends AnyVal

  case class WorkTypeTag(tag: String)// extends AnyVal

  case class Task(
    id: TaskId,
    parent: StoryId,
    pointsFraction: Double,
    dependents: Seq[TaskId],
    precedents: Seq[TaskId],
    workType: WorkTypeTag
  ) extends Work[TaskId]

  object Sprint {

    implicit val todoLens: Lens[Sprint, Seq[TaskOrStoryId]] = Lens[Sprint, Seq[TaskOrStoryId]](_.todo)(a => p => p.copy(todo = a))

    implicit val todoIdLens: Lens[TaskOrStoryId, TaskOrStoryId] = Lens[TaskOrStoryId, TaskOrStoryId](identity)(a => p => a)

    implicit val inprogressLens: Lens[Sprint, Seq[InProgressWork]] = Lens[Sprint, Seq[InProgressWork]](_.inprogress)(a => p => p.copy(inprogress = a))

    implicit val inprogressIdLens: Lens[InProgressWork, TaskOrStoryId] = Lens[InProgressWork, TaskOrStoryId](_.workId)(a => p => p.copy(workId = a))

    implicit val completedLens: Lens[Sprint, Seq[CompletedWork]] = Lens[Sprint, Seq[CompletedWork]](_.completed)(a => p => p.copy(completed = a))

    implicit val completedIdLens: Lens[CompletedWork, TaskOrStoryId] = Lens[CompletedWork, TaskOrStoryId](_.workId)(a => p => p.copy(workId = a))

    def withWorkTodo(sprint: Sprint, id: TaskOrStoryId): Sprint =
      (withoutWorkState[InProgressWork](_: Sprint, id))
        .andThen(withoutWorkState[CompletedWork](_: Sprint, id))
        .apply(withWorkState(sprint, id))

    def withWorkInProgress(sprint: Sprint, id: TaskOrStoryId, assignee: TeamMemberId, progressPoints: StoryPoints): Sprint =
      (withoutWorkState[TaskOrStoryId](_: Sprint, id))
        .andThen(withoutWorkState[CompletedWork](_: Sprint, id))
        .apply(withWorkState(sprint, InProgressWork(id, assignee, progressPoints)))

    def withWorkCompleted(sprint: Sprint, id: TaskOrStoryId, assignee: TeamMemberId, actualPoints: StoryPoints): Sprint =
      (withoutWorkState[TaskOrStoryId](_: Sprint, id))
        .andThen(withoutWorkState[InProgressWork](_: Sprint, id))
        .apply(withWorkState(sprint, CompletedWork(id, assignee, actualPoints)))

    def withWorkState[T](sprint: Sprint, state: T)(implicit lens: Lens[Sprint, Seq[T]], idLens: Lens[T, TaskOrStoryId]): Sprint =
      lens.modify { states =>
        val stateId = idLens.get(state)
        states
          .find(idLens.get(_) == stateId)
          .fold(states :+ state) { _ =>
            states.map {

              case s if idLens.get(s) == stateId =>
                state

              case s =>
                s

            }
          }
      }(sprint)

    def withoutWorkState[T](sprint: Sprint, withoutId: TaskOrStoryId)(implicit lens: Lens[Sprint, Seq[T]], idLens: Lens[T, TaskOrStoryId]): Sprint =
      lens.modify(_.filterNot(idLens.get(_) == withoutId))(sprint)

  }

  case class Sprint(
    todo: Seq[TaskOrStoryId],
    inprogress: Seq[InProgressWork],
    completed: Seq[CompletedWork]
  )

  case class ScrumProjectConfig(
    initialVelocityEstimate: StoryPoints,
    daysPerSprint: Int,
    estimateAccuracyProbability: Double,
    estimateVarianceRange: Int
  )

  trait KanbanStorySelectionStrategy {

    def nextStoryAllocation(workingPeriod: KanbanPeriod, prevPeriod: KanbanPeriod, project: KanbanProject): Option[(TeamMemberId, StoryId)]

  }

  case object KanbanPeriod {

    def sum(periods: Seq[KanbanPeriod]): StoryId Map StoryPoints =
      periods
        .flatMap(_.period.mapValues(_.progression))
        .groupBy(_._1)
        .mapValues(_.map(_._2).reduce(_ + _))

    case class StoryAllocation(teamMember: TeamMemberId, progression: StoryPoints)

    case class StoryAllocationConfig(executor: TeamMemberConfig, storyId: StoryId, outstandingPoints: StoryPoints, workTypeFractions: WorkTypeTag Map Double)


//    def allocate(allocation: StoryAllocationConfig, teamMemberPeriodPointAllowance: StoryPoints): (Option[StoryAllocation], Option[StoryPoints]) = {
//      val StoryAllocationConfig(config, sId, outstanding, workType) = allocation
//      val effectiveProgress =
//        TeamMemberConfig
//          .effectivePoints(config, outstanding, workType)
//          .map { outstanding =>
//            StoryPoints(Math.min(teamMemberPeriodPointAllowance.points, outstanding.points))
//          }
//
//      (
//        effectiveProgress
//          .map(StoryAllocation(config.name, _)),
//        effectiveProgress
//          .map(teamMemberPeriodPointAllowance.points  - _.points)
//          .filter(_ > 0)
//          .map(StoryPoints)
//      )
//    }



    def apply(allocation: Seq[StoryAllocationConfig], teamMemberPeriodPointAllowance: StoryPoints, existing: KanbanPeriod): KanbanPeriod = {

      @tailrec
      def inner(remainingStories: Seq[StoryAllocationConfig], unallocatedPoints: TeamMemberId Map StoryPoints, carry: StoryId Map KanbanPeriod.StoryAllocation): StoryId Map KanbanPeriod.StoryAllocation = {
          remainingStories match {

          case seq if seq.isEmpty =>
            carry

          case Seq(StoryAllocationConfig(config, sId, outstanding, workType), rest @ _*) =>

            val effectiveProgress =
              TeamMemberConfig
                .effectivePoints(config, outstanding, workType)
                .flatMap { outstanding =>
                  unallocatedPoints
                    .get(config.name)
                    .map(_.points)
                    .map(Math.min(_, outstanding.points))
                    .map(StoryPoints)
                }

             inner(
               remainingStories =
                 rest,
               unallocatedPoints =
                 effectiveProgress
                   .fold(unallocatedPoints){ progress =>
                     unallocatedPoints
                       .get(config.name)
                       .map(_.points - progress.points)
                       .filter(_ > 0)
                       .map(StoryPoints)
                       .map(config.name -> _)
                       .fold(unallocatedPoints - config.name)(unallocatedPoints + _)
                   },
               carry =
                 effectiveProgress
                   .map(StoryAllocation(config.name, _))
                   .map(sId -> _)
                   .fold(carry)(carry + _)
             )

        }
      }

      val existingPeriodAllocations =
        existing.period.values
          .map(v => v.teamMember -> v.progression)
          .foldLeft(Map.empty[TeamMemberId, StoryPoints]) {
            case (memo, (member, progression)) =>
              memo + (member -> memo.get(member).fold(progression)(_ + progression))
          }

      val allowance =
        allocation
          .map(_.executor.name)
          .map(_ -> teamMemberPeriodPointAllowance)
          .toMap
          .map { case (mId, points) =>
              mId -> existingPeriodAllocations.get(mId).fold(points)(points - _)
          }

      KanbanPeriod(inner(allocation, allowance, existing.period))

    }



  }

  case class KanbanPeriod(
    period: StoryId Map KanbanPeriod.StoryAllocation
  )

  object KanbanProject {

    def apply(
      teamMemberPeriodPointAllowance: StoryPoints,
      teamMembers: TeamMemberId Map TeamMemberConfig,
      stories: StoryId Map Story
    ): KanbanProject =
      KanbanProject(
        teamMemberPeriodPointAllowance,
        teamMembers,
        stories,
        completed = Seq.empty,
        periods = Seq.empty,
        graph =
          Graph(
            stories.values.toSeq.flatMap { story =>
              story.dependents.map(story.id ~> _)
            }:_*
          )
      )

  }

  case class KanbanProject(
    teamMemberPeriodPointAllowance: StoryPoints,
    teamMembers: TeamMemberId Map TeamMemberConfig,
    stories: StoryId Map Story,
    completed: Seq[StoryId],
    periods: Seq[KanbanPeriod],
    graph: Graph[StoryId, DiEdge]
  ) {

//    def nextStoryAllocation(workingPeriod: KanbanPeriod, prevPeriod: KanbanPeriod, project: KanbanProject): Option[(TeamMemberId, StoryId)]
  //    def apply(allocation: Seq[StoryAllocationConfig], teamMemberPeriodPointAllowance: TeamMemberId Map StoryPoints, existing: KanbanPeriod): KanbanPeriod = {
    def nextTick()(implicit strategy: KanbanStorySelectionStrategy): Option[KanbanProject] = {
      val prevPeriod = periods.lastOption.getOrElse(KanbanPeriod(Map.empty))
      def inner(period: KanbanPeriod, project: KanbanProject): Option[KanbanProject] = {
        strategy
          .nextStoryAllocation(period, prevPeriod, project)
          .flatMap { case (mId, sId) =>
            teamMembers
              .get(mId)
              .flatMap(tm => stories.get(sId).map(tm -> _))
          }.map {
            case (member, story) =>
              //    case class StoryAllocationConfig(executor: TeamMemberConfig, storyId: StoryId, outstandingPoints: StoryPoints, workTypeFractions: WorkTypeTag Map Double)
              project.copy(
                periods = project.periods :+
                  KanbanPeriod(
                    allocation =
                      Seq(StoryAllocationConfig(member, story.id, story.points, story.workTypeFractions)),
                    teamMemberPeriodPointAllowance =
                      teamMemberPeriodPointAllowance,
                    existing =
                      period
                  )
              )

          }
      }
      inner(KanbanPeriod(Map.empty), this)
    }


  }

  case class ScrumProject(
    config: ScrumProjectConfig,
    teamMembers: TeamMemberId Map TeamMemberConfig,
    stories: StoryId Map Story,
    tasks: TaskId Map Task,
    backlog: Backlog,
    sprint: Sprint = Sprint(Seq.empty, Seq.empty, Seq.empty),
    previousSprints: Seq[Sprint] = Seq.empty
  ) {

    def nextWorkRecommendation(dest: TeamMemberId): Option[TaskOrStoryId] =
      backlog.nextWorkRecommendation(dest, this)

    def withWorkAssigned(id: TaskOrStoryId, assignee: TeamMemberId): ScrumProject = copy(
      sprint =
        Sprint.withWorkInProgress(sprint, id, assignee, sprint.inprogress.find(_.workId == id).fold(StoryPoints(0))(_.consumedPoints)),
      backlog =
        backlog.withoutWork(id)
    )

    def withRecommendedWorkAssigned(assignee: TeamMemberId): Option[ScrumProject] =
      nextWorkRecommendation(assignee)
        .map(withWorkAssigned(_, assignee))

    def withUpdatedWork(work: TaskOrStory): ScrumProject =
      work
        .fold(task => copy(tasks = tasks + (task.id -> task)), story => copy(stories = stories + (story.id -> story)))
        .copy(backlog = backlog.withUpdatedWork(work))

    def withoutWork(id: TaskOrStoryId): ScrumProject =
      id
        .fold(id => copy(tasks = tasks - id), id => copy(stories = stories - id))
        .copy(backlog = backlog.withoutWork(id))

    def withWork(work: Either[Task, Story]): ScrumProject =
      work
        .fold(task => copy(tasks = tasks + (task.id -> task)), story => copy(stories = stories + (story.id -> story)))
        .copy(backlog = backlog.withWork(work))
  }

  //def play()

}
