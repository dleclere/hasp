package com.github.dleclere.hasp.jira

import org.scalatest.{FlatSpec, Matchers}
import com.github.dleclere.hasp.jira._
import com.github.dleclere.hasp.jira.JiraApi
import com.github.dleclere.hasp.jira.models._
import cats.effect.IO
import org.http4s.{BasicCredentials, Uri}
import org.http4s.client.blaze._
import org.http4s.headers.Authorization

import scala.concurrent.ExecutionContext.Implicits.global

class JiraApiSpec extends FlatSpec with Matchers {
  import com.github.dleclere.hasp.JiraServerConfig._

  import JiraApi._


  "A fetched issue's id" should "match requested issue id" in {
    val issueId = JiraIssueId(10002)
    fetchIssue(issueId).unsafeRunSync().id should be (issueId)
  }

  "A request for backlog issues" should "return more than one" in {

//    val boardId = JiraBoardId(1)
    val page = fetchBacklogIssues().unsafeRunSync()
    assert(page.results.length > 1)

  }


  "A request for the next backlog issues" should "return a valid next page" in {

//    val boardId = JiraBoardId(1)
    val page1: JiraResultPage[JiraIssue] =
      fetchBacklogIssues( 0, 1)
      .unsafeRunSync()
    val page2 = fetchNextPage[JiraIssue](page1).unsafeRunSync()
    page2.results should not be (empty)
    page2.results.head should not be (page1.results.head)
    page2.results.head should not be (page1.results.tail)

  }

  "A stream for the backlog issues" should "return all the issues" in {

//    val boardId = JiraBoardId(1)
    val page1Request = fetchBacklogIssues(0, 9)
    val page1: JiraResultPage[JiraIssue] =
      page1Request.unsafeRunSync()
    val allResults =
      streamResults(page1Request).compile.toVector.unsafeRunSync()
    allResults.length should be (page1.total)
  }

  "Changing the rank of a task" should "work" in {
    val results = fetchBacklogIssues().unsafeRunSync().results.sortBy(_.rank.getOrElse("zzzzzzz"))
    val last = results(1).id
    val head = results.head.id
    rankIssueBefore(last, head).unsafeRunSync()
    val resultsv2 = fetchBacklogIssues().unsafeRunSync().results.sortBy(_.rank.getOrElse("zzzzzzz"))
    resultsv2.head.id should be (last)
    resultsv2(1).id should be (head)
  }

}
