package com.github.dleclere.hasp.backlog

import com.github.dleclere.hasp.Domain._
import com.github.dleclere.hasp._


case class SimpleBacklog(
  backlog: Seq[TaskOrStoryId]
) extends Backlog {

  def nextWorkRecommendation(dest: TeamMemberId, project: ScrumProject): Option[TaskOrStoryId] =
    backlog.headOption

  def withUpdatedWork(work: Either[Task, Story]): Backlog =
    this

  def withoutWork(id: TaskOrStoryId): Backlog =
    copy(backlog.filterNot(_ == id))

  def withWork(work: Either[Task, Story]): Backlog =
    copy(backlog :+ workIdExtractor(work))


}
