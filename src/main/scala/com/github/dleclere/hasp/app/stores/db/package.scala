package com.github.dleclere.hasp.app.stores
import com.github.dleclere.hasp.app.config.AppRepoConfig
import doobie._
import doobie.implicits._
import cats._
import cats.effect._
import cats.implicits._

package object db {

  def setup(ta: Transactor.Aux[IO, Unit]): IO[AppRepoConfig] = {
    for {
      _ <- Schemas.definitions.transact(ta)

    } yield new AppRepoConfig {
      override val appRepo: AppRepo = new DoobieAppRepo(ta)
      override val scheduleRepo: ScheduleRepo = new DoobieScheduleRepo(ta)
    }
  }

}
