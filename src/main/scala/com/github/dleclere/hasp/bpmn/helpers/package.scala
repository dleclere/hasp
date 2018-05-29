package com.github.dleclere.hasp.bpmn

import scala.language.implicitConversions

package object helpers {

  type ParserSig = xml.Node => Seq[BaseElement]

  object BpmnXmlFactorySig {

    def unapply[R <: BaseRef](sig: BpmnXmlFactorySig[R]): Option[(Seq[FilterMatcher], FactoryInfo[R] => Option[BaseElement])] = {

      def inner: BpmnXmlFactorySig[R] => (Seq[FilterMatcher], FactoryInfo[R] => Option[BaseElement]) = {

        case BpmnXmlFactorySigMatcher(matcher, next) =>
          val (matchers, applicator) = inner(next)
          (matcher +: matchers, applicator)

        case BpmnXmlFactorySigApplicator(applicator) =>
          (Seq.empty, applicator)

      }
      Some(inner(sig))
    }


  }

//  implicit def factory2Applicator[R <: BaseRef](fn: FactoryInfo[R] => Option[BaseElement]): BpmnXmlFactorySigApplicator[R] =
//    BpmnXmlFactorySigApplicator(fn)

  sealed trait BpmnXmlFactorySig[R <: BaseRef] {

    def apply: FactoryInfo[R] => Option[BaseElement]

    def applicator: BpmnXmlFactorySigApplicator[R]

  }
  case class BpmnXmlFactorySigMatcher[R <: BaseRef](matcher: FilterMatcher, next: BpmnXmlFactorySig[R]) extends BpmnXmlFactorySig[R] {

    def apply: FactoryInfo[R] => Option[BaseElement] = next.apply

    def applicator: BpmnXmlFactorySigApplicator[R] = next.applicator

  }
  case class BpmnXmlFactorySigApplicator[R <: BaseRef](factory: FactoryInfo[R] => Option[BaseElement]) extends BpmnXmlFactorySig[R] {

    def apply: FactoryInfo[R] => Option[BaseElement] = factory

    def applicator: BpmnXmlFactorySigApplicator[R] = this

  }

  case class ElementBuilder(filter: ElementFilter, refFactory: String => BaseRef, elFactory: (xml.Node, ElementBuilder, String Map BaseRef) => Option[BaseElement], altBuilder: Option[ElementBuilder]) {

    def |(otherBuilder: ElementBuilder): ElementBuilder =
      copy(
        altBuilder = Some(altBuilder.fold(otherBuilder)(_ | otherBuilder))
      )

  }

  object FactoryInfo {

    def apply[R <: BaseRef](node: xml.Node, refs: String Map BaseRef, refFactory: String => R, elBuilder: => ElementBuilder): FactoryInfo[R] =
      new FactoryInfo[R](
        id = readAttribute(node, "id").map(refFactory),
        label = node.label,
        attributes = node.attributes
          .flatMap { attr =>
            attr.value.headOption.map(_.text).map(attr.key -> _)
          }.toMap,
        node = node,
        children = node.child.flatMap(parseNode(_, refs, elBuilder)),
        refs = refs
      )


  }

  class FactoryInfo[R <: BaseRef](
    val id: Option[R],
    val label: String,
    val attributes: String Map String,
    val node: xml.Node,
    children: => Seq[BaseElement],
    val refs: String Map BaseRef
  ) {

    def childElements: Seq[BaseElement] = children

  }

  sealed trait FilterMatcher {
    def test(node: xml.Node): Boolean
  }

  case class NodeLabelWhiteList(allowed: String*) extends FilterMatcher {

    def test(node: xml.Node): Boolean =
      allowed.map(_.toLowerCase) contains node.label.toLowerCase

  }

  case class NodeLabelEndsWith(allowed: String) extends FilterMatcher {

    def test(node: xml.Node): Boolean =
      node.label.toLowerCase endsWith allowed.toLowerCase

  }

  case class ElementFilter(matcher: FilterMatcher*) {

    def test(node: xml.Node): Boolean =
      matcher.forall(_ test node)

  }

  def readAttribute(node: xml.Node, key: String): Option[String] =
    node.attribute(key).flatMap(_.headOption).map(_.text)

  private def readSequenceFlows(node: xml.Node, label: String): Seq[SequenceFlowRef] =
    (node \ s"$label").map(_.text.replaceAllLiterally("\n", "").trim).map(SequenceFlowRef)

  def readIncomingSequenceFlows(node: xml.Node): Seq[SequenceFlowRef] =
    readSequenceFlows(node, "incoming")

  def readOutgoingSequenceFlows(node: xml.Node): Seq[SequenceFlowRef] =
    readSequenceFlows(node, "outgoing")

  def parseNode(node: xml.Node, refs: => String Map BaseRef, builder: => ElementBuilder): Option[BaseElement] =
    findElementBuilder(node, builder).flatMap(_.elFactory(node, builder, refs))

  def parse(nodes: xml.NodeSeq, builder: => ElementBuilder): BpmnDiagram = {
    val refs: String Map BaseRef =
      (nodes \\ "_")
        .filter(_.prefix == "semantic")
        .flatMap { node =>
          findElementBuilder(node, builder).flatMap { b =>
            node.attribute("id").headOption
              .map(_.text)
              .map(id => id -> b.refFactory(id))
          }
        }.toMap
    BpmnDiagram(nodes.filter(_.prefix == "semantic")
      .flatMap(_.child.flatMap(parseNode(_, refs, builder))))
  }

}
