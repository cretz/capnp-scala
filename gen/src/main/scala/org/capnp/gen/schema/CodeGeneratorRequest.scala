package org.capnp.gen.schema

import org.capnp.model._

case class CodeGeneratorRequest() extends Struct(0xd07378ede1f9cc60L, 0, 2) {
  def nodes = structSeq[Node](0, Node)
  def nodes_=(v: Seq[Node]) = structSeq_=(0, v)
  
  def requestedFiles = structSeq[CodeGeneratorRequest.RequestedFile](1, CodeGeneratorRequest.RequestedFile)
  def requestedFiles_=(v: Seq[CodeGeneratorRequest.RequestedFile]) = structSeq_=(1, v)
}

object CodeGeneratorRequest extends StructObject[CodeGeneratorRequest] {
  case class RequestedFile() extends Struct(0xcfea0eb02e810062L, 8, 2) {
    def id = uint64Field(0)
    def id_=(v: BigInt) = uint64Field_=(0, v)
    
    def filename = textField(0)
    def filename_=(v: String) = textField_=(0, v)
    
    def imports = structSeq[RequestedFile.Import](1, RequestedFile.Import)
    def imports_=(v: Seq[RequestedFile.Import]) = structSeq_=(1, v)
  }

  object RequestedFile extends StructObject[CodeGeneratorRequest.RequestedFile] {
    case class Import() extends Struct(0xae504193122357e5L, 8, 1) {
      def id = uint64Field(0)
      def id_=(v: BigInt) = uint64Field_=(0, v)
      
      def name = textField(0)
      def name_=(v: String) = textField_=(0, v)
    }
  
    object Import extends StructObject[Import]
  }
}