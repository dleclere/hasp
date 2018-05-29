package com.github.dleclere.hasp

import com.github.dleclere.hasp.jira.models.{JiraBoard, JiraBoardId, JiraProjectKey, JiraResultPage, decodeJiraResultsPage}
import cats.Monad
import cats.effect.IO
import fs2.Stream
import io.circe.Decoder
import org.http4s.MediaType.`application/json`
import org.http4s.client.Client
import org.http4s.headers._
import org.http4s.{Credentials, EntityEncoder, Header, Method, Uri}
import org.http4s.circe._
import org.http4s.Method._
import org.http4s.client.dsl.io._


package object jira {

  type BaseApiConfig = UriAwareApiConfig with ClientAwareApiConfig with AuthorizationAwareApiConfig

  trait JiraSpecialFieldConfigLike {

    def rankFieldId: String
    def estimationFieldId: String

  }

  case class JiraSpecialFieldConfig(
    rankFieldId: String,
    estimationFieldId: String
  ) extends JiraSpecialFieldConfigLike

  trait UriAwareApiConfig {
    val root: Uri
  }

  trait ClientAwareApiConfig {
    val client: Client[IO]
  }

  trait AuthorizationAwareApiConfig {
    val authorization: Authorization
  }


  trait BoardAwareApiConfig {
    val board: JiraBoardId
  }

  trait ProjectAwareApiConfig {
    val project: JiraProjectKey
  }

  case class ApiConnectionConfig(
    client: Client[IO],
    authorization: Authorization,
    root: Uri,
  ) extends UriAwareApiConfig with ClientAwareApiConfig with AuthorizationAwareApiConfig

  case class InitialApiConfig(
    client: Client[IO],
    authorization: Authorization,
    root: Uri,
    board: JiraBoardId,
    project: JiraProjectKey
  ) extends UriAwareApiConfig with ClientAwareApiConfig with AuthorizationAwareApiConfig with BoardAwareApiConfig with ProjectAwareApiConfig

  object ApiConfig {

    def apply(initial: BaseApiConfig with BoardAwareApiConfig with ProjectAwareApiConfig, specialFieldConfig: JiraSpecialFieldConfig): ApiConfig = {
      import initial._
      ApiConfig(
        client = client,
        authorization = authorization,
        root = root,
        board = board,
        project = project,
        specialFieldConfig = specialFieldConfig
      )
    }

    def apply(config: BaseApiConfig with BoardAwareApiConfig with ProjectAwareApiConfig): IO[ApiConfig] = {
      import config._
      implicit val iconfig = config
      expect[JiraBoard](
        root / "rest" / "agile" / "1.0" / "board"/ s"${board.id}" / "configuration"
      ) map { board =>
        println("Making a new API Config!")
        ApiConfig(
          config,
          specialFieldConfig = JiraSpecialFieldConfig(
            rankFieldId = board.rankingFieldId,
            estimationFieldId = board.estimationFieldId
          )
        )
      }
    }

  }

  case class ApiConfig(
    client: Client[IO],
    authorization: Authorization,
    root: Uri,
    board: JiraBoardId,
    project: JiraProjectKey,
    specialFieldConfig: JiraSpecialFieldConfig,
  ) extends UriAwareApiConfig with ClientAwareApiConfig with AuthorizationAwareApiConfig with JiraSpecialFieldConfigLike with BoardAwareApiConfig with ProjectAwareApiConfig  {

    def rankFieldId: String = specialFieldConfig.rankFieldId
    def estimationFieldId: String = specialFieldConfig.estimationFieldId

  }

  def basicRest(implicit config: UriAwareApiConfig): Uri =
    config.root / "rest" / "api" / "2"

  def agileRest(implicit config: UriAwareApiConfig): Uri =
    config.root / "rest" / "agile" / "1.0"

  def expectFetch[T](uri: Uri)(implicit dc: Decoder[T], apiConfig: BaseApiConfig): IO[T] =
    expect[T](uri, GET)

  def expect[T](uri: Uri, method: Method with NoBody = GET)(implicit dc: Decoder[T], apiConfig: BaseApiConfig): IO[T] =
    apiConfig.client.expect(
      method(
        uri,
        apiConfig.authorization,
        Accept(`application/json`)
      )
    )(jsonOf[IO, T])

  def fetchWithPaginatedResults[R](uri: Uri, startAt: Option[Int], maxResults: Option[Int])(implicit dc: Decoder[R], apiConfig: BaseApiConfig): IO[JiraResultPage[R]] = {
    val params = Seq("startAt" -> startAt, "maxResults" -> maxResults)
    val keys = params.map(_._1)
    val quri = uri.copy(
      query =
        params.flatMap(kv => kv._2.map(v => kv._1 -> Some(v.toString)))
          .foldLeft(uri.query.filterNot(keys contains _._1))(_ :+ _)
    )
    implicit val rdc = decodeJiraResultsPage[R](quri)
    expect[JiraResultPage[R]](
      quri
    )
  }

  def fetchNextPage[T](previous: JiraResultPage[T])(implicit apiConfig: BaseApiConfig): IO[JiraResultPage[T]] = {
    val start = previous.startAt + previous.results.length
    fetchWithPaginatedResults(previous.uri, Some(start), Some(previous.maxResults))(previous.decoder, apiConfig)
  }

  def streamResults[T](first: IO[JiraResultPage[T]])(implicit apiConfig: BaseApiConfig): Stream[IO, T] =
    Stream
      .eval(first)
      .flatMap { page =>
        val results = Stream(page.results: _*)
        if (page.startAt + page.results.length >= page.total)
          results
        else
          results ++ streamResults(fetchNextPage(page))
      }

}
