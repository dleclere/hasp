package com.github.dleclere.hasp.jira.models

import java.net.URL

import com.github.dleclere.hasp.jira.JiraSpecialFieldConfig
import org.scalatest._
import com.github.dleclere.hasp.jira.models._
import io.circe.parser.decode

import scala.io.Source

class DecodeSpec extends FlatSpec with Matchers  {
  val rankField = "customfield_10011"
  val estimationField = "customfield_10011"
  implicit val config = JiraSpecialFieldConfig(
    rankFieldId = rankField,
    estimationFieldId = estimationField
  )

//  val provider = new JiraIssueModelDecoderProvider(rankField, estimationField)
//  import provider._

  val decoded1 = decode[JiraIssue]{
    Source.fromURL(getClass.getClassLoader.getResource("jira/rest/examples/issue/CGP-24-v1.json")).getLines().mkString("\n")
  }

  val decoded2 = decode[JiraIssue]{
    Source.fromURL(getClass.getClassLoader.getResource("jira/rest/examples/issue/CGP-3-v1.json")).getLines().mkString("\n")
  }

  println(s"DECODED 1!!!! $decoded1")
  println(s"DECODED 2!!!! $decoded2")

}
