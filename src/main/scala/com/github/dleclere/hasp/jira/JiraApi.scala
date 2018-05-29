package com.github.dleclere.hasp.jira

import cats.effect._
import org.http4s._
import org.http4s.client.blaze._
import cats._
import cats.implicits._
import org.http4s.Uri
import org.http4s.client.dsl.io._
import org.http4s.headers._
import org.http4s.MediaType._
import models._
import org.http4s.client._
import org.http4s.client.dsl.io._
import org.http4s.Method._
import org.http4s.circe._
import Utils._
import cats.data.EitherT
import io.circe.{Decoder, Json}
import fs2.{Pure, Stream}
import io.circe.parser._
import io.circe._


//issuetype not in (Epic) AND resolution = Unresolved and status != Closed AND (sprint is empty OR sprint != "Sample Sprint 2") ORDER BY RANK
object JiraApi {

  private val backlogIssuesJql = "issuetype not in (Epic) AND resolution = Unresolved and status != Closed AND (sprint is empty OR sprint NOT IN openSprints()) ORDER BY RANK"

  ///rest/api/2/user/assignable/multiProjectSearch?projectKeys=LP |
  
  def fetchUsers(implicit config: ApiConfig): IO[Seq[JiraUser]] =
    expectFetch[Seq[JiraUser]](
      basicRest / "user" / "assignable" / "multiProjectSearch" :? ("projectKeys" := config.project.key)
    )
  
  def fetchIssue(id: JiraIssueId)(implicit config: ApiConfig): IO[JiraIssue] =
    expectFetch[JiraIssue](
      basicRest / "issue" / s"${id.id}"
    )

  def fetchBacklogIssueRanks(startAt: Int = 0, maxResultsPerPage: Int = 50)(implicit config: ApiConfig): Stream[IO, (JiraIssueId, String)] =
    fetchAndPickIssueFields(Seq(config.rankFieldId), startAt, maxResultsPerPage)
      .map { obj =>
        (obj \\ config.rankFieldId)
          .headOption
          .flatMap(_.asString)
          .flatMap { name =>
            (obj \\ "id")
              .headOption
              .flatMap(_.asString)
              .map(_.toInt)
              .map(JiraIssueId)
              .map(_ -> name)
          }
      }
      .unNone

  def fetchBacklogIssueAssignees(startAt: Int = 0, maxResultsPerPage: Int = 50)(implicit config: ApiConfig): Stream[IO, (JiraIssueId, Option[JiraUserName])] = {
    fetchAndPickIssueFields(Seq("assignee"), startAt, maxResultsPerPage)
      .map { obj =>
        (obj \\ "id")
          .headOption
          .flatMap(_.asString)
          .map(_.toInt)
          .map(JiraIssueId).map { issueId =>
          issueId ->
            (obj \\ "name")
              .headOption
              .flatMap(_.asString.map(JiraUserName))
        }

      }
      .unNone
  }
  

  def fetchBacklogIssues(startAt: Int = 0, maxResultsPerPage: Int = 50)(implicit config: ApiConfig): IO[JiraResultPage[JiraIssue]] = {
    val uri = agileRest / "board" / s"${config.board.id}" / "issue" :? ("jql" := backlogIssuesJql)
    fetchWithPaginatedResults[JiraIssue](uri, Some(startAt), Some(maxResultsPerPage))
  }

  def fetchAllBoardIssues(startAt: Int = 0, maxResultsPerPage: Int = 50)(implicit config: ApiConfig): IO[JiraResultPage[JiraIssue]] =
    fetchWithPaginatedResults[JiraIssue](agileRest / "board" / s"${config.board.id}" / "issue", Some(startAt), Some(maxResultsPerPage))

  def updateIssueAssignee(issue: JiraIssueId, assignee: JiraUserName)(implicit config: ApiConfig): IO[Status] = {
    import Json._
    updateIssueFields(issue, false, "assignee" -> obj(
      "name" -> Json.fromString(assignee.name)
    ))
  }


  def rankIssueBefore(issue: JiraIssueId, before: JiraIssueId)(implicit config: ApiConfig): IO[Boolean] = {
    rankIssue(issue, before, "Before")
  }

  def rankIssueAfter(issue: JiraIssueId, after: JiraIssueId)(implicit config: ApiConfig): IO[Boolean] = {
    rankIssue(issue, after, "After")
  }

  private def rankIssue(issue: JiraIssueId, other: JiraIssueId, relative: String)(implicit config: ApiConfig): IO[Boolean] = {
    val body =
      parse {
        s"""{
           |  "issues": [
           |     "${issue.id}"
           |  ],
           |  "rank${relative}Issue": "${other.id}",
           |  "rankCustomFieldId": ${config.rankFieldId.split('_').last}
           |}
        """.stripMargin
      }
    config.client.expect[String] {
      PUT(
        agileRest / "issue" / "rank",
        body.toOption.get,
        config.authorization,
        Accept(`application/json`)
      )
    }.map(_ => true)
  }

  private def fetchAndPickIssueFields(pickFields: Seq[String], startAt: Int = 0, maxResultsPerPage: Int = 50)(implicit config: ApiConfig): Stream[IO, Json] = streamResults {
    val uri = agileRest / "board" / s"${config.board.id}" / "issue" :? ("jql" := backlogIssuesJql, "fields" := pickFields.mkString(","))
    fetchWithPaginatedResults[Json](uri, Some(startAt), Some(maxResultsPerPage))
  }

  private def updateIssueFields(issue: JiraIssueId, editComplex: Boolean, fields: (String, Json)*)(implicit config: ApiConfig): IO[Status] = {
    import Json._
    val uri = basicRest / "issue" / issue.id.toString :? ("notifyUsers" := "false")
    val cplx = if (editComplex) "edit" else "set"
    val body =
      obj(
        "update" ->
          obj(
            fields
              .map { case (k, v) =>
                k ->
                  v.arrayOrObject(
                    or =
                      arr(obj("set" -> v)),
                    jsonArray = ar =>
                      fromValues(ar.map(av => obj("add" -> av))),
                    jsonObject = o =>
                      arr(obj(cplx -> fromJsonObject(o)))
                  )
              }: _*
          )
      )

    config.client.status(
      PUT(
        uri,
        body,
        config.authorization,
        Accept(`application/json`),
        `Content-Type`(`application/json`)
      )
    )

  }

}