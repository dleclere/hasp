package com.github.dleclere.hasp.app.service

import com.github.dleclere.hasp.app.Hasp.workspace
import com.github.dleclere.hasp.app.config.{AppRepoConfig, HttpClientConfig, JiraAppWorkspaceConfig}
import com.github.dleclere.hasp.app.models._
import com.github.dleclere.hasp.app.{models, stores}
import com.github.dleclere.hasp.app.stores._
import com.github.dleclere.hasp.jira.models._
import com.github.dleclere.hasp.utils._
import doobie._
import doobie.implicits._
import cats._
import cats.effect._
import cats.implicits._
import io.circe.syntax._
import org.http4s.circe._
import cats.effect._
import org.http4s._
import org.http4s.dsl.io.{/, _}
import io.circe._
import io.circe.generic.auto._
import com.github.dleclere.hasp.app.workflow._
import cats.effect._
import com.github.dleclere.hasp.gridopt._
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.io._

object Service {

  implicit val wsDecoder = jsonOf[IO, AppUser]

  implicit val rsDecoder = jsonOf[IO, Resource]

  def routes(implicit config: AppRepoConfig with HttpClientConfig): HttpService[IO] =  {
    import config.appRepo._
    import config.scheduleRepo._
    def withWS[T](name: String)(block: AppRepoConfig with HttpClientConfig with JiraAppWorkspaceConfig => IO[T]) =
      jiraWSCfg(JiraAppWorkspaceName(name)).flatMap(block)

    HttpService[IO] {

      case GET -> Root / "api" / workspace / "users" / userId => withWS(workspace) { implicit wsConfig =>
        println(s"Fetching user ${userId} in ${workspace}")
        for {
          user <- getUser(AppUserId(userId)).attempt.map(u => {println(s"USERFETCHED ${u}"); u}).map(_.right.get)
          response <- Ok(user.asJson).attempt.map(u => {println(s"RESPONSE PARSED ${u}"); u}).map(_.right.get)
        } yield response
      }

      case req @ (POST | PUT) -> Root / "api" / workspace / "user" / userId => withWS(workspace) { implicit wsConfig =>
        for {
          user <- req.as[AppUser]
          id = AppUserId(userId)
          _ <- setUser(id, user)
          result <- getUser(id)
          response <- Ok(result.asJson)
        } yield response
      }

      case GET -> Root / "api" / workspace / "resources" / IntVar(resourceId) => withWS(workspace) { implicit wsConfig =>
        for {
          resource <- getResource(ResourceId(resourceId))
          response <- Ok(resource.asJson)
        } yield response
      }

      case req @ (POST | PUT) -> Root / "api" / workspace / "resources" / IntVar(resourceId) => withWS(workspace) { implicit wsConfig =>
        for {
          resource <- req.as[Resource].attempt.map(u => {println(s"Resouce request ${u}"); u}).map(_.right.get)
          id = ResourceId(resourceId)
          _ <- setResource(id, resource).attempt.map(u => {println(s"Resource set ${u}"); u}).map(_.right.get)
          result <- getResource(id).attempt.map(u => {println(s"request fetched ${u}"); u}).map(_.right.get)
          response <- Ok(result.asJson).attempt.map(u => {println(s"json encoded ${u}"); u}).map(_.right.get)
        } yield response
      }

      case (POST | PUT) -> Root / "api" / workspace / "optimise-schedule" => withWS(workspace) { implicit wsConfig =>
        for {
          schedule <- OptimiseJiraBoard.optimise(JiraAppWorkspaceName(workspace))
          response <- Ok(schedule.asJson)
        } yield response
      }

    }
  }


//  val jsonService = HttpService[IO] {
//    case req @ POST -> Root / "hello" =>
//      for {
//        // Decode a User request
//        user <- req.as[User]
//        // Encode a hello response
//        resp <- Ok(Hello(user.name).asJson)
//      } yield (resp)
//  }


}
