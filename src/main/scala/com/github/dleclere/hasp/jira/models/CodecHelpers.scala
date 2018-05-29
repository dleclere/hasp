package com.github.dleclere.hasp.jira.models

import io.circe.{ACursor, Decoder, DecodingFailure}

object CodecHelpers {

  implicit class ExACursor(cursor: ACursor) {

    def downFieldPath(field: String*): ACursor =
      field.foldLeft(cursor)(_.downField(_))

    def \(field: String): ACursor =
      cursor downField field

    def safeAs[T <: Option[_]](implicit dc: Decoder[T]): Decoder.Result[T] =
      cursor
        .as[T]
        .swap
        .flatMap(_ => Left[T, DecodingFailure](None.asInstanceOf[T]))
        .swap

  }

}
