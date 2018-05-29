package com.github.dleclere.hasp

import com.github.dleclere.hasp.jira.{ApiConfig, InitialApiConfig}
import com.github.dleclere.hasp.jira.models._
import cats.effect.IO
import org.http4s.client.blaze.Http1Client
import org.http4s.headers.Authorization
import org.http4s.{BasicCredentials, Uri}

object JiraServerConfig {
  val user = "hasp"
  val apiKey = "jira"
  val jiraUri = Uri.uri("http://localhost:8080")
  val board = JiraBoardId(1)
  val project = JiraProjectKey("LP")


  val client = Http1Client[IO]().unsafeRunSync

  val iconfig = InitialApiConfig(
    client = client,
    authorization = Authorization(BasicCredentials(user, apiKey)),
    root = jiraUri,
    board = board,
    project = project
  )

  implicit val aconfig = ApiConfig(iconfig).unsafeRunSync()

}
