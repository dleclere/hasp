package com.github.dleclere.hasp.jira

import org.http4s.{Query, Uri}

object Utils {

  type QStr = (String, Option[String])

  implicit class EQStr(key: String) {

    def := (value: String): QStr =
      (key, Some(value))

  }

  implicit class EUri(uri: Uri) {

    def :?(query: QStr*): Uri =
      uri.copy(query = Query(query: _*))

  }

}
