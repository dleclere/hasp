package com.github.dleclere.hasp.bpmn

object BuilderRegister {

  val builder: ElementBuilder =
    Seq(
      Process,
      Activity,
      Gateway,
      Event,
      SequenceFlow,
      LaneSet,
      Lane
    )
      .map(_.builder)
      .reduceLeft(_ | _)

}
