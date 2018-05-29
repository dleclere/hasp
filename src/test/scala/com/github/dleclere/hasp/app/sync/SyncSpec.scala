package com.github.dleclere.hasp.app.sync

import com.github.dleclere.hasp.jira.JiraApi.{fetchBacklogIssues, rankIssueBefore}
import com.github.dleclere.hasp.jira._
import com.github.dleclere.hasp.jira.models._
import cats.effect.IO
import org.http4s.client.blaze._
import org.http4s.headers.Authorization
import org.http4s.{BasicCredentials, Uri}
import org.scalatest.{FlatSpec, Matchers}

class SyncSpec extends FlatSpec with Matchers {
  import com.github.dleclere.hasp.JiraServerConfig._

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