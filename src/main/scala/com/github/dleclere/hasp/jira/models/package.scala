package com.github.dleclere.hasp.jira

import io.circe.generic.JsonCodec
import io.circe.syntax._
import io.circe.generic.semiauto._
import io.circe.generic.extras._
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor}
import org.http4s.Uri
import com.github.dleclere.hasp.utils.ExtEither

package object models {
  import CodecHelpers._

  implicit val config: Configuration = Configuration.default

  sealed trait JiraId

  case class JiraIssueId(id: Int) extends JiraId

  case class JiraIssueLinkId(id: Int) extends JiraId

  case class JiraUserName(name: String) extends JiraId

  case class JiraBoardId(id: Int) extends JiraId

  case class JiraProjectKey(key: String) extends JiraId

  def idDecoder[T, Id](const: T => Id, idField: String = "id")(implicit tdc: Decoder[T]): Decoder[Id] =
    (c: HCursor) =>
      ((c \ idField).as[T] orElse c.value.as[T]) map const

  def idEncoder[T, Id](dconst: Id => T)(implicit tec: Encoder[T]): Encoder[Id] =
    Encoder.instance[Id](id => tec(dconst(id)))

  implicit val jiraIssueIdDecoder: Decoder[JiraIssueId] = idDecoder(JiraIssueId)
  implicit val jiraIssueLinkIdDecoder: Decoder[JiraIssueLinkId] = idDecoder(JiraIssueLinkId)
  implicit val jiraUserIdDecoder: Decoder[JiraUserName] = idDecoder(JiraUserName, "name")
  implicit val jiraBoardIdDecoder: Decoder[JiraBoardId] = idDecoder(JiraBoardId)
  implicit val jiraProjectIdDecoder: Decoder[JiraProjectKey] = idDecoder(JiraProjectKey)


  implicit val jiraIssueIdEncoder: Encoder[JiraIssueId] = idEncoder(_.id)
  implicit val jiraIssueLinkIdEncoder: Encoder[JiraIssueLinkId] = idEncoder(_.id)
  implicit val jiraUserIdEncoder: Encoder[JiraUserName] = idEncoder(_.name)
  implicit val jiraBoardIdEncoder: Encoder[JiraBoardId] = idEncoder(_.id)
  implicit val jiraProjectIdEncoder: Encoder[JiraProjectKey] = idEncoder(_.key)

  implicit val decodeJiraBoard: Decoder[JiraBoard] = (c: HCursor) => for {
    id <- (c \ "id").as[JiraBoardId]
    name <- (c \ "name").as[String]
    tpe <- (c \ "type").as[String]
    est = c \"estimation" \ "field"
    estFieldId <- (est \ "fieldId").as[String]
    estFieldName <- (est \ "displayName").as[String]
    rankFieldId <- (c \ "ranking" \ "rankCustomFieldId").as[Int]
  } yield
    JiraBoard(id, tpe, name, estFieldId, estFieldName, s"customfield_$rankFieldId")


  sealed trait JiraModel {

    val id: JiraId

  }

  implicit def decodeJiraIssue(implicit config: JiraSpecialFieldConfigLike): Decoder[JiraIssue] = {
    import config._

    def issueLinkDecoder(parent: JiraIssueId): Decoder[JiraIssueLink] = (c: HCursor) => for {
      tpe <- (c \ "type").as[JiraIssueLinkType]
      id <- (c \ "id").as[JiraIssueLinkId]
      mkLink = JiraIssueLink(_: JiraIssueId, tpe, _: JiraIssueId, id)
      outward = (c \ "outwardIssue").as[JiraIssueId].map(mkLink(_, parent))
      inward = (c \ "inwardIssue").as[JiraIssueId].map(mkLink(parent, _))
      link <- outward orElse inward
    } yield link

    c: HCursor => for {
      id <- (c \ "id").as[JiraIssueId]
      key <- (c \ "key").as[String]
      fields = c \ "fields"
      parent <- (fields \ "parent").as[Option[JiraIssueId]]
      labels <- (fields \ "labels").as[Seq[String]]
      assignee <- (fields \ "assignee" \ "name").safeAs[Option[JiraUserName]]
      issueLinks <- (fields \ "issuelinks").as[Seq[JiraIssueLink]](Decoder.decodeSeq(issueLinkDecoder(id)))
      rank <- (fields \ rankFieldId).as[Option[String]]
      estimation <- (fields \ config.estimationFieldId).as[Option[Int]]
      subTasks <- (fields \ "subtasks").as[Seq[JiraIssueId]]
    } yield
      JiraIssue(id, key, parent, labels, assignee, issueLinks, rank, estimation, subTasks)

  }

  def decodeJiraResultsPage[R](uri: Uri)(implicit dc: Decoder[R]): Decoder[JiraResultPage[R]] = (c: HCursor) => {
    val metaFields = Seq("expand", "startAt", "maxResults", "total")
    for {
      expand <- (c \ "expand").as[String]
      startAt <- (c \ "startAt").as[Int]
      maxResults <- (c \ "maxResults").as[Int]
      total <- (c \ "total").as[Int]
      resultsField = c.keys.toSeq.flatten.filterNot(metaFields.contains).head
      results <- (c \ resultsField).as[Seq[R]](Decoder.decodeSeq(dc))
    } yield
      JiraResultPage(uri, expand, startAt, maxResults, total, results, dc)
  }

  case class JiraResultPage[R](
    uri: Uri,
    expand: String,
    startAt: Int,
    maxResults: Int,
    total: Int,
    results: Seq[R],
    decoder: Decoder[R]
  )

//issuetype not in (Epic) AND resolution = Unresolved and status != Closed AND (sprint is empty OR sprint NOT IN openSprints()) ORDER BY RANK

  case class JiraIssue(
    id: JiraIssueId,
    key: String,
    parent: Option[JiraIssueId],
    labels: Seq[String],
    assignee: Option[JiraUserName],
    issueLinks: Seq[JiraIssueLink],
    rank: Option[String],
    estimation: Option[Int],
    subtasks: Seq[JiraIssueId]
  ) extends JiraModel

  @JsonCodec(decodeOnly = true) case class JiraIssueLinkType(
    name: String
  )

  @JsonCodec(decodeOnly = true) case class JiraIssueLink(
    destinationIssueId: JiraIssueId,
    issueLinkType: JiraIssueLinkType,
    sourceIssueId: JiraIssueId,
    id: JiraIssueLinkId
  ) extends JiraModel

  @JsonCodec(decodeOnly = true) case class JiraUser(
    displayName: String,
    name: JiraUserName,
    self: String,
    emailAddress: String,
    key: String
  ) extends JiraModel {

    override val id: JiraUserName = name

  }

  @ConfiguredJsonCodec case class JiraChangeLogItem(
    from: Option[String],
    to: Option[String],
    fromString: Option[String],
    @JsonKey("toString") destString: Option[String],
    fieldType: String,
    fieldId: String,
    field: String
  )

  @JsonCodec(decodeOnly = true) case class JiraChangeLog(
    id: String,
    items: Seq[JiraChangeLogItem]
  )

  /*
    {
      "timestamp": 1526084032700,
     "issueLink": { },
      "webhookEvent": "issuelink_created"
    }
   */

  case class JiraBoard(
    id: JiraBoardId,
    tpe: String,
    name: String,
    estimationFieldId: String,
    estimationFieldName: String,
    rankingFieldId: String
  ) extends JiraModel


  case class CreationWebhookEvent(timestamp: Long, model: JiraModel)

  case class UpdateWebhookEvent(timestamp: Long, model: JiraModel, changeLog: JiraChangeLog)

  case class DeleteWebhookEvent(timestamp: Long, id: JiraId)


}
