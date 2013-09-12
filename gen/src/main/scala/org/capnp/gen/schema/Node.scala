package org.capnp.gen.schema

import org.capnp.model
import org.capnp.model._

sealed abstract class Node extends Struct(0xe682ab4cf923a417L, 40, 5) with Union {
  def id = uint64Field(0)
  def id_=(v: BigInt) = uint64Field_=(0, v)
  
  def displayName = textField(0)
  def displayName_=(v: String) = textField_=(0, v)
  
  def displayNamePrefixLength = uint64Field(64)
  def displayNamePrefixLength_=(v: BigInt) = uint64Field_=(64, v)
  
  def scopeId = uint64Field(128)
  def scopeId_=(v: BigInt) = uint64Field_=(128, v)
  
  def nestedNodes = structSeq[Node.NestedNode](1, Node.NestedNode)
  def nestedNodes_=(v: Seq[Node.NestedNode]) = structSeq_=(1, v)
  
  def annotations = structSeq[Annotation](2, Annotation)
  def annotations_=(v: Seq[Annotation]) = structSeq_=(2, v)
}
object Node extends AnonUnionObject[Node] {
  protected val unionTagBitOffset = 96L
  protected val cases = Map(
    0 -> File,
    1 -> Struct,
    2 -> Enum,
    3 -> Interface,
    4 -> Const,
    5 -> Annotation
  )
  
  case class File() extends Node {
    protected val unionTag = 0
  }
  
  case class Struct() extends Node with Group {
    protected val unionTag = 1

    def dataWordCount = uint16Field(112)
    def dataWordCount_=(v: Int) = uint16Field_=(112, v)
    
    def pointerCount = uint16Field(192)
    def pointerCount_=(v: Int) = uint16Field_=(192, v)
    
    def preferredListEncoding = enumField[ElementSize.Value](208, ElementSize.apply)
    def preferredListEncoding_=(v: ElementSize.Value) = enumField_=(208, v)
    
    def isGroup = boolField(224)
    def isGroup_=(v: Boolean) = boolField_=(224, v)
    
    def discriminantCount = uint16Field(240)
    def discriminantCount_=(v: Int) = uint16Field_=(240, v)
    
    def discriminantOffset = uint32Field(256)
    def discriminantOffset_=(v: Long) = uint32Field_=(256, v)
    
    def fields = structSeq[Field](3, Field)
    def fields_=(v: Seq[Field]) = structSeq_=(3, v)
  }
  
  case class Enum() extends Node with Group {
    protected val unionTag = 2
    
    def enumerants = structSeq[Enumerant](3, Enumerant)
    def enumerants_=(v: Seq[Enumerant]) = structSeq_=(3, v)
  }
  
  case class Interface() extends Node with Group {
    protected val unionTag = 3
    
    def methods = structSeq[Method](3, Method)
    def methods_=(v: Seq[Method]) = structSeq_=(3, v)
  }
  
  case class Const() extends Node with Group {
    protected val unionTag = 4
    
    def `type` = structField[Type](3, Type)
    def type_=(v: Type) = structField_=(3, v)
    
    def value = structField[Value](4, Value)
    def value_=(v: Type) = structField_=(4, v)
  }
  
  case class Annotation() extends Node with Group {
    protected val unionTag = 5
    
    def `type` = structField[Type](3, Type)
    def type_=(v: Type) = structField_=(3, v)
    
    def targetsFile = boolField(112)
    def targetsFile_=(v: Boolean) = boolField_=(112, v)
    
    def targetsConst = boolField(113)
    def targetsConst_=(v: Boolean) = boolField_=(113, v)
    
    def targetsEnum = boolField(114)
    def targetsEnum_=(v: Boolean) = boolField_=(114, v)
    
    def targetsEnumerant = boolField(115)
    def targetsEnumerant_=(v: Boolean) = boolField_=(115, v)
    
    def targetsStruct = boolField(116)
    def targetsStruct_=(v: Boolean) = boolField_=(116, v)
    
    def targetsField = boolField(117)
    def targetsField_=(v: Boolean) = boolField_=(117, v)
    
    def targetsUnion = boolField(118)
    def targetsUnion_=(v: Boolean) = boolField_=(118, v)
    
    def targetsGroup = boolField(119)
    def targetsGroup_=(v: Boolean) = boolField_=(119, v)
    
    def targetsInterface = boolField(120)
    def targetsInterface_=(v: Boolean) = boolField_=(120, v)
    
    def targetsMethod = boolField(121)
    def targetsMethod_=(v: Boolean) = boolField_=(121, v)
    
    def targetsParam = boolField(122)
    def targetsParam_=(v: Boolean) = boolField_=(122, v)
    
    def targetsAnnotation = boolField(123)
    def targetsAnnotation_=(v: Boolean) = boolField_=(123, v)   
  }
  
  case class NestedNode() extends model.Struct(0x9aad50a41f4af45fL, 8, 1) {
    def name = textField(0)
    def name_=(v: String) = textField_=(0, v)
    
    def id = uint64Field(0)
    def id_=(v: BigInt) = uint64Field_=(0, v)
  }
  object NestedNode extends StructObject[NestedNode]
}