package com.github.dleclere.hasp.bpmn.helpers

import com.github.dleclere.hasp.bpmn.{BaseElement, BpmnDiagram}

trait ParseHelpers {

  def parse(nodes: xml.NodeSeq, builder: => ElementBuilder): BpmnDiagram =
    com.github.dleclere.hasp.bpmn.helpers.parse(nodes, builder)
}
