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
      val c = m.root(CodeGeneratorRequest)
      println(c)
      println(c.nodes.size)
      println(c.requestedFiles.size)
      1 mustEqual 1
    }
  }
}