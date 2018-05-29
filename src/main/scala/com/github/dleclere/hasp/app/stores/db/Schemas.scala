package com.github.dleclere.hasp.app.stores.db

import doobie._
import doobie.implicits._
import cats._
import cats.data._
import cats.effect.IO
import cats.implicits._

/*
def codecMeta[A : Encoder : Decoder : TypeTag]: Meta[A] =
  Meta[Json].xmap[A](
    _.as[A].fold[A](throw _, identity),
    _.asJson
  )
 */
object Schemas {

  val definitions =
    for {
      _ <- sql"""
            CREATE TABLE IF NOT EXISTS AppUser (
              workspace     VARCHAR NOT NULL,
              id            VARCHAR NOT NULL,
              jiraName      VARCHAR NOT NULL,
              resource      INT NOT NULL,
              PRIMARY KEY (workspace, id)
            )
          """.update.run
      _ <- sql"""
            CREATE TABLE IF NOT EXISTS JiraAppWorkspace (
              name          VARCHAR NOT NULL UNIQUE,
              apiUsername   VARCHAR NOT NULL,
              apiPassword   VARCHAR NOT NULL,
              root          VARCHAR NOT NULL UNIQUE,
              board         INT NOT NULL,
              project       VARCHAR NOT NULL,
              PRIMARY KEY (name)
            )
          """.update.run
      _ <- sql"""
            CREATE TABLE IF NOT EXISTS Resource (
              workspace     VARCHAR NOT NULL,
              id            SERIAL NOT NULL UNIQUE,
              capacity      JSONB NOT NULL,
              PRIMARY KEY (workspace, id)
            )
          """.update.run
    } yield ()




}
