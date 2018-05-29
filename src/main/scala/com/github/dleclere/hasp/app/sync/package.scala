package com.github.dleclere.hasp.app

import com.github.dleclere.hasp.jira.JiraApi._
import com.github.dleclere.hasp.jira._
import com.github.dleclere.hasp.jira.models.{JiraIssueId, JiraUserName}
import com.github.dleclere.hasp.utils.VecSync._
import cats.effect.IO
import cats.implicits._

package object sync {

  def syncJiraIssueAssignees(
    issues: Map[JiraIssueId, JiraUserName]
  )(implicit config: ApiConfig): IO[Unit] =
    fetchBacklogIssueAssignees()
      .collect { case (issue, user) if issues.contains(issue) && (user.isEmpty || user.get != issues(issue)) =>
        updateIssueAssignee(issue, issues(issue)).start
      }
      .compile
      .toList
      .flatMap(_.foldLeft(IO.unit)((m, s) => m.flatMap(_ => s.map(_.join))))

  def syncJiraIssueRanks(
    tasks: Seq[JiraIssueId]
  )(implicit config: ApiConfig): IO[Boolean] =
    fetchBacklogIssueRanks()
      .compile
      .toVector
      .map(_.sortBy(_._2))
      .flatMap { remoteIssues =>
        val remoteOrder = remoteIssues.map(_._1.id)
        val localOrder = tasks.map(_.id).toVector
        syncOps(remoteOrder, localOrder)
          .collect {

            case MoveBefore(item, before) =>
              rankIssueBefore(JiraIssueId(item), JiraIssueId(before))

            case MoveAfter(item, after) =>
              rankIssueAfter(JiraIssueId(item), JiraIssueId(after))

          }.reduceOption((a, b) => a.flatMap(a => b.map(a && _))).getOrElse(IO.pure(true))

      }

}
