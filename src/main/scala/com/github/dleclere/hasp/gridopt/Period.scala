package com.github.dleclere.hasp.gridopt

import scala.language.implicitConversions


case class Period(num: Int) extends AnyVal

trait PeriodOps {

  implicit def int2Period(num: Int): Period =
    Period(num)

  def add: Period => Period => Period =
    a => b => Period(a.num + b.num)

  def increment(period: Period): Period =
    Period(period.num + 1)

}
