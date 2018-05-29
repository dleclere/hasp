package com.github.dleclere.hasp.app.stores.db.connections

import cats.effect.IO
import doobie.util.transactor.Transactor.Aux

object ConnectionManager {

  case class Config(
    driver: String,
    uri: String,
    user: String,
    pass: String
  )

}

trait ConnectionManager {

  val transactor: Aux[IO, Unit]

}
