package com.github.dleclere.hasp.bpmn


case class DocumentationRef(id: String) extends BaseRef

object Documentation {

  object MimeType extends Enumeration {

    val `text/plain` = Value

    private val valueMap = values.map(v => v.toString.toLowerCase -> v).toMap

    def apply(str: String): Value =
      apply(Option(str))

    def apply(str: Option[String]): Value =
      str
        .map(_.toLowerCase)
        .flatMap(valueMap.get)
        .getOrElse(`text/plain`)

  }

  type MimeType = MimeType.Value

  object DocBody {
    import MimeType._

    def apply(mimeType: MimeType, content: Array[Byte]): DocBody = mimeType match {

      case `text/plain` =>
        StringDocBody(content)
    }

  }

  sealed trait DocBody {

    val content: Any

    def asString: String

  }

  object StringDocBody {

    def apply(content: Array[Byte]): StringDocBody =
      StringDocBody(content.map(_.toChar).mkString)

  }

  case class StringDocBody(content: String) extends DocBody {

    def asString: String = content

  }

}

case class Documentation(
  id: Option[DocumentationRef],
  textFormat: Documentation.MimeType,
  body: Documentation.DocBody
) extends BaseElement