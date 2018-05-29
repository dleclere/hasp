package com.github.dleclere.hasp.app.stores.db.connections

import com.github.dleclere.hasp.app.stores.db.connections.ConnectionManager.Config
import cats.effect.IO
import doobie.util.transactor.Transactor
import doobie.util.transactor.Transactor.Aux

class JDBCManager(
  config: Config
) extends ConnectionManager {

  val transactor: Aux[IO, Unit] = Transactor.fromDriverManager[IO](
    s"${config.driver}",//"", // fully-qualified driver class name
    s"${config.uri}", // connect URL
    s"${config.user}",                 // user
    s"${config.pass}"                // password
  )
}
