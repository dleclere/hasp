package com.github.dleclere.hasp.bpmn

trait RefBuilder[T <: BaseRef] {

  def build(id: String): T

}
