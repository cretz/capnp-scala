package org.capnp.gen.schema

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import org.capnp.model.Message

@RunWith(classOf[JUnitRunner])
class CodeGeneratorRequestSpec extends Specification {
  "CodeGeneratorRequest model"should {
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
      def nodeCheck(n: Node) {
        n.displayName must beAnInstanceOf[String]
        n.displayNamePrefixLength must beAnInstanceOf[BigInt]
        n.scopeId must beAnInstanceOf[BigInt]
        n.nestedNodes foreach { n =>
          n.name must beAnInstanceOf[String]
          n.id must beAnInstanceOf[BigInt]
        }
        n.annotations foreach { a =>
          a.id must beAnInstanceOf[BigInt]
          a.value map { _ must beAnInstanceOf[Value] }
        }
      }
      c.nodes.foreach(nodeCheck(_))
      1 mustEqual 1
    }
  }
}