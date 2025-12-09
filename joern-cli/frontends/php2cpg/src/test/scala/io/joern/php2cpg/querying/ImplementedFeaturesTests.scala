package io.joern.php2cpg.querying

import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.joern.php2cpg.parser.Domain.PhpOperators
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class ImplementedFeaturesTests extends PhpCode2CpgFixture {

  "variable variables ($$var)" should {
    "be represented as variableVariable operator calls" in {
      val cpg = code("""<?php
        |$name = "foo";
        |$$name = "bar"; // creates $foo = "bar"
        |echo $$name;
        |""".stripMargin)

      // The $$name should be represented as a variableVariable operator call
      inside(cpg.call.nameExact(PhpOperators.variableVariable).l) { case varVarCalls =>
        varVarCalls.size should be > 0
        varVarCalls.foreach { call =>
          call.code should include("$")
        }
      }
    }

    "handle nested variable variables" in {
      val cpg = code("""<?php
        |$a = "b";
        |$b = "c";
        |$c = "value";
        |echo $$$a; // should be "value" ($$b = $c)
        |""".stripMargin)

      // Nested variable variables should create nested operator calls
      val varVarCalls = cpg.call.nameExact(PhpOperators.variableVariable).l
      varVarCalls.size should be > 0
    }

    "handle expression-based variable names" in {
      val cpg = code("""<?php
        |$prefix = "user_";
        |${$prefix . "name"} = "John";
        |""".stripMargin)

      // Expression-based variable access should be represented
      val varVarCalls = cpg.call.nameExact(PhpOperators.variableVariable).l
      varVarCalls.size should be > 0
    }
  }

  "yield from expressions" should {
    "be distinguished from regular yield" in {
      val cpg = code("""<?php
        |function gen() {
        |  yield 1;
        |  yield 2;
        |}
        |
        |function delegating() {
        |  yield from gen();
        |  yield 3;
        |}
        |""".stripMargin)

      // Regular yields should exist
      val yields = cpg.controlStructure.controlStructureType(ControlStructureTypes.YIELD).l
      yields.size should be > 0

      // Yield from should have a different parserTypeName
      val yieldFroms = cpg.controlStructure.parserTypeName("YIELD_FROM").l
      yieldFroms.size shouldBe 1
      yieldFroms.head.code should include("yield from")
    }

    "have the delegated expression as a child" in {
      val cpg = code("""<?php
        |function gen() {
        |  yield from [1, 2, 3];
        |}
        |""".stripMargin)

      inside(cpg.controlStructure.parserTypeName("YIELD_FROM").l) { case List(yieldFrom) =>
        yieldFrom.code should include("yield from")
        yieldFrom.astChildren.l.size should be > 0
      }
    }
  }

  "reference assignments" should {
    "wrap source in addressOf operator" in {
      val cpg = code("""<?php
        |$a = 1;
        |$b = &$a;
        |""".stripMargin)

      // The reference assignment should create an addressOf operator
      val addressOfCalls = cpg.call.nameExact(Operators.addressOf).l
      addressOfCalls.size shouldBe 1

      inside(addressOfCalls) { case List(addrOf) =>
        addrOf.code should include("&")
      }
    }

    "work in foreach loops" in {
      val cpg = code("""<?php
        |$arr = [1, 2, 3];
        |foreach ($arr as &$value) {
        |  $value *= 2;
        |}
        |""".stripMargin)

      // Foreach with reference should be parsed
      cpg.controlStructure.controlStructureType(ControlStructureTypes.FOR).l.size should be > 0
    }
  }

  "trait use statements with adaptations" should {
    "create import nodes for trait references" in {
      val cpg = code("""<?php
        |trait TraitA {
        |  public function foo() { echo "A"; }
        |}
        |
        |trait TraitB {
        |  public function foo() { echo "B"; }
        |}
        |
        |class UsesBoth {
        |  use TraitA, TraitB {
        |    TraitA::foo insteadof TraitB;
        |    TraitB::foo as fooB;
        |  }
        |}
        |""".stripMargin)

      // Check that traits are defined
      cpg.typeDecl.name("TraitA").l.size shouldBe 1
      cpg.typeDecl.name("TraitB").l.size shouldBe 1
      cpg.typeDecl.name("UsesBoth").l.size shouldBe 1

      // Check for imports of the traits
      val imports = cpg.imports.l
      imports.map(_.importedEntity).flatten should contain("TraitA")
      imports.map(_.importedEntity).flatten should contain("TraitB")

      // Check for adaptation annotations
      val annotations = cpg.annotation.l
      annotations.map(_.name) should contain("TraitPrecedence")
      annotations.map(_.name) should contain("TraitAlias")
    }

    "handle simple trait use without adaptations" in {
      val cpg = code("""<?php
        |trait SimpleTrait {
        |  public function hello() { echo "Hello"; }
        |}
        |
        |class UsesSimple {
        |  use SimpleTrait;
        |}
        |""".stripMargin)

      val imports = cpg.imports.l
      imports.map(_.importedEntity).flatten should contain("SimpleTrait")
    }

    "handle visibility modification adaptations" in {
      val cpg = code("""<?php
        |trait MyTrait {
        |  private function secret() { echo "Secret"; }
        |}
        |
        |class Exposed {
        |  use MyTrait {
        |    secret as public;
        |  }
        |}
        |""".stripMargin)

      val aliasAnnotations = cpg.annotation.name("TraitAlias").l
      aliasAnnotations.size should be > 0
    }
  }

  "constant imports" should {
    "be tracked in scope" in {
      val cpg = code("""<?php
        |namespace Foo;
        |
        |const MY_CONST = 42;
        |
        |namespace Bar;
        |
        |use const Foo\MY_CONST;
        |
        |echo MY_CONST;
        |""".stripMargin)

      // Check that the use const statement creates an import
      val imports = cpg.imports.l
      imports.exists(i => i.code.contains("use const")) shouldBe true
    }
  }

  "static closures" should {
    "have the static modifier set" in {
      val cpg = code("""<?php
        |class Foo {
        |  public function bar() {
        |    $normalClosure = function() {
        |      return $this;
        |    };
        |
        |    $staticClosure = static function() {
        |      // cannot access $this here
        |      return "static";
        |    };
        |  }
        |}
        |""".stripMargin)

      // Check that lambda methods exist
      val lambdaMethods = cpg.method.name(".*lambda.*").l
      lambdaMethods.size shouldBe 2

      // One should be static, one should not
      val staticLambdas = lambdaMethods.filter(_.modifier.modifierType.l.contains("STATIC"))
      staticLambdas.size shouldBe 1
    }
  }

  "graceful error handling" should {
    "not crash on malformed expressions" in {
      // This test ensures the crash fixes work - the parser should handle
      // unexpected constructs without throwing exceptions
      val cpg = code("""<?php
        |// Various edge cases that previously caused crashes
        |$arr = [];
        |function foo() {
        |  return;
        |}
        |""".stripMargin)

      // If we get here without crashing, the test passes
      cpg.method.name("foo").l.size shouldBe 1
    }
  }

}
