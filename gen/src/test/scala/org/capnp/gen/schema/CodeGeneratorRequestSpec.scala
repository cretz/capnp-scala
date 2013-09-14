package org.capnp.gen.schema

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import org.capnp.model.Message
import org.capnp.model.DynObject

@RunWith(classOf[JUnitRunner])
class CodeGeneratorRequestSpec extends Specification {
  "CodeGeneratorRequest model" should {
    "read the schema itself" in {
      val m = Message.readAll(getClass.getResourceAsStream("schema.capnp.bin"))
      val c = m.root(CodeGeneratorRequest).get
      // Check requested files first
      c.requestedFiles.size mustEqual 1
      val rf = c.requestedFiles.head
      rf.id mustEqual BigInt("12195682960037147353")
      rf.filename mustEqual "capnproto-c++-0.3.0/src/capnp/schema.capnp"
      rf.imports.size mustEqual 1
      rf.imports.head.id mustEqual BigInt("13688829037717245569")
      rf.imports.head.name mustEqual "c++.capnp"
      // Now nodes
      c.nodes.size mustEqual 28
      // Let's just check every type I guess
      
      c.nodes.foreach(nodeCheck(_))
      1 mustEqual 1
    }
  }
  
  def nodeCheck(n: Node) {
    n.displayName must beAnInstanceOf[String]
    n.displayNamePrefixLength must beAnInstanceOf[BigInt]
    n.scopeId must beAnInstanceOf[BigInt]
    n.nestedNodes foreach { n =>
      n.name must beAnInstanceOf[String]
      n.id must beAnInstanceOf[BigInt]
    }
    n.annotations map(annotationCheck(_))
    n match {
      case _: Node.File => ()
      case n: Node.Struct =>
        n.dataWordCount must beAnInstanceOf[Integer]
        n.pointerCount must beAnInstanceOf[Integer]
        n.preferredListEncoding must beAnInstanceOf[ElementSize.Value]
        n.isGroup must beAnInstanceOf[java.lang.Boolean]
        n.discriminantCount must beAnInstanceOf[Integer]
        n.fields map { f =>
          f.name must beAnInstanceOf[String]
          f.codeOrder must beAnInstanceOf[Integer]
          f.annotations map(annotationCheck(_))
          f.discriminantValue must beAnInstanceOf[Integer]
          f.ordinal match {
            case _: f.Ordinal.Implicit => ()
            case o: f.Ordinal.Explicit => o.value must beAnInstanceOf[Integer]
          }
          f match {
            case f: Field.Slot =>
              f.offset must beAnInstanceOf[java.lang.Long]
              f.`type` map(typeCheck(_))
              f.value map(valueCheck(_))
            case f: Field.Group => f.typeId must beAnInstanceOf[BigInt]
          }
        }
      case n: Node.Enum => n.enumerants map { e =>
        e.name must beAnInstanceOf[String]
        e.codeOrder must beAnInstanceOf[Integer]
        e.annotations map(annotationCheck(_))
      }
      case n: Node.Interface => n.methods map { m =>
        m.name must beAnInstanceOf[String]
        m.codeOrder must beAnInstanceOf[Integer]
        m.params map { p =>
          p.name must beAnInstanceOf[String]
          p.`type` map(typeCheck(_))
          p.defaultValue map(valueCheck(_))
          p.annotations map(annotationCheck(_))
        }
        m.requiredParamCount must beAnInstanceOf[Integer]
        m.returnType map(typeCheck(_))
        m.annotations map(annotationCheck(_))
      }
      case n: Node.Const =>
        n.`type` map(typeCheck(_))
        n.value map(valueCheck(_))
      case n: Node.Annotation =>
        n.`type` map(typeCheck(_))
        n.targetsFile must beAnInstanceOf[java.lang.Boolean]
        n.targetsConst must beAnInstanceOf[java.lang.Boolean]
        n.targetsEnum must beAnInstanceOf[java.lang.Boolean]
        n.targetsEnumerant must beAnInstanceOf[java.lang.Boolean]
        n.targetsStruct must beAnInstanceOf[java.lang.Boolean]
        n.targetsField must beAnInstanceOf[java.lang.Boolean]
        n.targetsUnion must beAnInstanceOf[java.lang.Boolean]
        n.targetsGroup must beAnInstanceOf[java.lang.Boolean]
        n.targetsInterface must beAnInstanceOf[java.lang.Boolean]
        n.targetsMethod must beAnInstanceOf[java.lang.Boolean]
        n.targetsParam must beAnInstanceOf[java.lang.Boolean]
        n.targetsAnnotation must beAnInstanceOf[java.lang.Boolean]
    }
  }
  
  def annotationCheck(a: Annotation) {
    a.id must beAnInstanceOf[BigInt]
    a.value map(valueCheck(_))
  }
  
  def valueCheck(v: Value): Unit = v match {
    case _: Value.Void => ()
    case v: Value.Bool => v.value must beAnInstanceOf[java.lang.Boolean]
    case v: Value.Int8 => v.value must beAnInstanceOf[java.lang.Byte]
    case v: Value.Int16 => v.value must beAnInstanceOf[java.lang.Short]
    case v: Value.Int32 => v.value must beAnInstanceOf[Integer]
    case v: Value.Int64 => v.value must beAnInstanceOf[java.lang.Long]
    case v: Value.UInt8 => v.value must beAnInstanceOf[java.lang.Short]
    case v: Value.UInt16 => v.value must beAnInstanceOf[Integer]
    case v: Value.UInt32 => v.value must beAnInstanceOf[java.lang.Long]
    case v: Value.UInt64 => v.value must beAnInstanceOf[BigInt]
    case v: Value.Float32 => v.value must beAnInstanceOf[java.lang.Float]
    case v: Value.Float64 => v.value must beAnInstanceOf[java.lang.Double]
    case v: Value.Text => v.value must beAnInstanceOf[String]
    case v: Value.Data => v.value must beAnInstanceOf[Seq[Byte]]
    case v: Value.List => v.value.map(_ must beAnInstanceOf[DynObject])
    case v: Value.Enum => v.value must beAnInstanceOf[Integer]
    case v: Value.Struct => v.value.map(_ must beAnInstanceOf[DynObject])
    case _: Value.Interface => ()
    case v: Value.Object => v.value.map(_ must beAnInstanceOf[DynObject])
  }
  
  def typeCheck(t: Type): Unit = t match {
    case t: Type.List => t.elementType map(typeCheck(_))
    case t: Type.Enum => t.typeId must beAnInstanceOf[BigInt]
    case t: Type.Struct => t.typeId must beAnInstanceOf[BigInt]
    case t: Type.Interface => t.typeId must beAnInstanceOf[BigInt]
    case _ => ()
  }
}