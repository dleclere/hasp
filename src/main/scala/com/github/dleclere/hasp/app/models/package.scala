package com.github.dleclere.hasp.app

import com.github.dleclere.hasp.gridopt.ResourceId
import com.github.dleclere.hasp.jira.models.{JiraBoardId, JiraProjectKey, JiraUserName}
import org.http4s.Uri
import io.circe._
import io.circe.generic.auto._
import cats.implicits._
import io.circe.generic.JsonCodec
import io.circe.syntax._
import com.github.dleclere.hasp.gridopt._


package object models {

  @JsonCodec case class AppUserId(id: String)

  @JsonCodec case class JiraAppWorkspaceName(name: String)

  @JsonCodec case class AppUser(
    id: AppUserId,
    jiraName: JiraUserName,
    resource: ResourceId
  )

  case class JiraAppWorkspace(
    name: JiraAppWorkspaceName,
    apiUsername: String,
    apiPassword: String,
    root: Uri,
    board: JiraBoardId,
    project: JiraProjectKey
  )

}
