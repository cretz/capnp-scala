package org.capnp.gen.schema

import org.capnp.model
import org.capnp.model._

case class CodeGeneratorRequest extends Struct(0xd07378ede1f9cc60L) {
  def nodes = seqField[Node](0)
  def nodes_=(v: Seq[Node]) = seqField_=(0, v)
  
  def requestedFiles = seqField[CodeGeneratorRequest.RequestedFile](1)
  def requestedFiles_=(v: Seq[CodeGeneratorRequest.RequestedFile]) = seqField_=(1, v)
}
object CodeGeneratorRequest {
  case class RequestedFile extends Struct(0xcfea0eb02e810062L) {
    def id = uint64Field(0)
    def id_=(v: BigInt) = uint64Field_=(0, v)
    
    def filename = textField(0)
    def filename_=(v: String) = textField_=(0, v)
    
    def imports = seqField[RequestedFile.Import](1)
    def imports_=(v: Seq[RequestedFile.Import]) = seqField_=(1, v)
  }
  object RequestedFile {
    case class Import extends Struct(0xae504193122357e5L) {
      def id = uint64Field(0)
      def id_=(v: BigInt) = uint64Field_=(0, v)
      
      def name = textField(0)
      def name_=(v: String) = textField_=(0, v)
    }
  }
}