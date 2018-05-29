package com.github.dleclere.hasp.bpmn


case class ExpressionRef(id: String) extends BaseRef

trait Expression extends BaseElement with MixedContentElement {

}

case class SimpleExpression(
  id: Option[ExpressionRef],
  body: String
) extends Expression

case class FormalExpression(
  id: Option[ExpressionRef]
)

/*
<xsd:complexType name="tFormalExpression">
<xsd:complexContent>
<xsd:extension base="tExpression">
<xsd:attribute name="language" type="xsd:anyURI" use="optional"/>
<xsd:attribute name="evaluatesToTypeRef" type="xsd:QName"/>
</xsd:extension>
</xsd:complexContent>
</xsd:complexType>
 */

