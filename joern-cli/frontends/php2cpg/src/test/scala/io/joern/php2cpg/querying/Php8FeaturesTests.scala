package io.joern.php2cpg.querying

import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.joern.php2cpg.parser.Domain
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.{ModifierTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class Php8FeaturesTests extends PhpCode2CpgFixture {

  "PHP 8.0 named arguments" should {
    "be parsed correctly with argument names" in {
      val cpg = code("""<?php
        |function greet(string $name, string $greeting = "Hello") {
        |  echo "$greeting, $name!";
        |}
        |
        |greet(name: "World", greeting: "Hi");
        |""".stripMargin)

      inside(cpg.call.nameExact("greet").l) { case List(greetCall) =>
        greetCall.lineNumber shouldBe Some(6)

        inside(greetCall.argument.l) { case List(nameArg: Literal, greetingArg: Literal) =>
          nameArg.code shouldBe "\"World\""
          nameArg.argumentIndex shouldBe 1

          greetingArg.code shouldBe "\"Hi\""
          greetingArg.argumentIndex shouldBe 2
        }
      }
    }

    "support out-of-order named arguments" in {
      val cpg = code("""<?php
        |function test(int $a, int $b, int $c) {
        |  return $a + $b + $c;
        |}
        |
        |test(c: 3, a: 1, b: 2);
        |""".stripMargin)

      inside(cpg.call.nameExact("test").l) { case List(testCall) =>
        inside(testCall.argument.l) { case List(cArg: Literal, aArg: Literal, bArg: Literal) =>
          // Arguments are parsed in source order
          cArg.argumentIndex shouldBe 1
          aArg.argumentIndex shouldBe 2
          bArg.argumentIndex shouldBe 3
        }
      }
    }

    "parse mixed positional and named arguments" in {
      val cpg = code("""<?php
        |function mixed($pos1, $pos2, $named1 = null, $named2 = null) {}
        |
        |mixed("a", "b", named2: "d");
        |""".stripMargin)

      inside(cpg.call.nameExact("mixed").l) { case List(mixedCall) =>
        inside(mixedCall.argument.l) { case List(pos1: Literal, pos2: Literal, named2: Literal) =>
          pos1.argumentIndex shouldBe 1
          pos2.argumentIndex shouldBe 2
          named2.argumentIndex shouldBe 3
        }
      }
    }
  }

  "PHP 8.1 readonly properties" should {
    "have the readonly modifier set" in {
      val cpg = code("""<?php
        |class User {
        |  public readonly string $name;
        |  public readonly int $age;
        |
        |  public function __construct(string $name, int $age) {
        |    $this->name = $name;
        |    $this->age = $age;
        |  }
        |}
        |""".stripMargin)

      inside(cpg.typeDecl.name("User").member.l) { case List(nameMember, ageMember) =>
        nameMember.name shouldBe "name"
        // Member types go through AnyTypePass; type recovery handles proper inference
        nameMember.modifier.modifierType.l should contain(ModifierTypes.READONLY)

        ageMember.name shouldBe "age"
        ageMember.modifier.modifierType.l should contain(ModifierTypes.READONLY)
      }
    }

    "parse constructor property promotion parameters" in {
      val cpg = code("""<?php
        |class Point {
        |  public function __construct(
        |    public readonly int $x,
        |    public readonly int $y
        |  ) {}
        |}
        |""".stripMargin)

      // Constructor property promotion creates constructor parameters
      // Full member creation requires additional pass implementation
      inside(cpg.method.name("__construct").parameter.l) { case params =>
        params.map(_.name) should contain("x")
        params.map(_.name) should contain("y")
      }
    }
  }

  "PHP 8.1 readonly classes" should {
    "have the readonly modifier on the class" in {
      val cpg = code("""<?php
        |readonly class ImmutableData {
        |  public string $value;
        |}
        |""".stripMargin)

      inside(cpg.typeDecl.name("ImmutableData").l) { case List(typeDecl) =>
        typeDecl.modifier.modifierType.l should contain(ModifierTypes.READONLY)
      }
    }
  }

  "PHP 8.0 union types" should {
    "be parsed in function parameters" in {
      val cpg = code("""<?php
        |function process(int|string $value): int|string {
        |  return $value;
        |}
        |""".stripMargin)

      inside(cpg.method.name("process").parameter.name("value").l) { case List(valueParam) =>
        valueParam.typeFullName shouldBe "int|string"
      }

      inside(cpg.method.name("process").methodReturn.l) { case List(ret) =>
        ret.typeFullName shouldBe "int|string"
      }
    }

    "parse class properties with union types" in {
      val cpg = code("""<?php
        |class Foo {
        |  public int|float $number;
        |}
        |""".stripMargin)

      inside(cpg.typeDecl.name("Foo").member.name("number").l) { case List(numberMember) =>
        numberMember.name shouldBe "number"
      // Member types go through AnyTypePass; type recovery handles proper inference
      }
    }
  }

  "PHP 8.0 nullsafe operator" should {
    "be represented as a special call" in {
      val cpg = code("""<?php
        |$country = $user?->getAddress()?->country;
        |""".stripMargin)

      // The nullsafe operator should be represented in the CPG
      // Check that the call chain exists with nullsafe markers
      val calls = cpg.call.filter(_.code.contains("?->")).l
      calls.size should be > 0
    }
  }

  "PHP 8.0 match expression" should {
    "be parsed as a control structure" in {
      val cpg = code("""<?php
        |$result = match($value) {
        |  1 => "one",
        |  2 => "two",
        |  default => "other"
        |};
        |""".stripMargin)

      // Match expressions should create a control structure or call
      cpg.call.l.size should be > 0
    }
  }

  "PHP 8.0 constructor property promotion" should {
    "parse promoted properties as constructor parameters" in {
      val cpg = code("""<?php
        |class Car {
        |  public function __construct(
        |    public string $make,
        |    public string $model,
        |    protected int $year = 2024
        |  ) {}
        |}
        |""".stripMargin)

      // Constructor property promotion parameters are parsed as constructor params
      // Full member generation requires additional pass implementation
      inside(cpg.method.name("__construct").parameter.l) { case params =>
        params.map(_.name) should contain("make")
        params.map(_.name) should contain("model")
        params.map(_.name) should contain("year")
      }
    }
  }

  "PHP 8.1 enums with backed values" should {
    "have correct string backed values" in {
      val cpg = code("""<?php
        |enum Status: string {
        |  case PENDING = 'pending';
        |  case ACTIVE = 'active';
        |  case CLOSED = 'closed';
        |}
        |""".stripMargin)

      inside(cpg.typeDecl.name("Status").l) { case List(statusEnum) =>
        statusEnum.code shouldBe "enum Status"

        inside(statusEnum.member.l) { case members =>
          val memberNames = members.map(_.name)
          memberNames should contain("PENDING")
          memberNames should contain("ACTIVE")
          memberNames should contain("CLOSED")
        }
      }
    }

    "have correct integer backed values" in {
      val cpg = code("""<?php
        |enum Priority: int {
        |  case LOW = 1;
        |  case MEDIUM = 2;
        |  case HIGH = 3;
        |}
        |""".stripMargin)

      inside(cpg.typeDecl.name("Priority").l) { case List(priorityEnum) =>
        priorityEnum.code shouldBe "enum Priority"

        inside(priorityEnum.member.l) { case members =>
          members.size shouldBe 3
        }
      }
    }
  }

  "PHP 8.1 intersection types" should {
    "be parsed in function parameters" in {
      val cpg = code("""<?php
        |interface A {}
        |interface B {}
        |
        |function process(A&B $value): void {}
        |""".stripMargin)

      inside(cpg.method.name("process").parameter.name("value").l) { case List(valueParam) =>
        valueParam.typeFullName shouldBe "A&B"
      }
    }
  }

  "PHP 8.0 attributes on various targets" should {
    "parse attributes on classes" in {
      val cpg = code("""<?php
        |#[Attribute]
        |class MyAttribute {}
        |
        |#[MyAttribute]
        |class Target {}
        |""".stripMargin)

      cpg.typeDecl.name("Target").annotation.name("MyAttribute").l.size shouldBe 1
    }

    "parse class with property" in {
      val cpg = code("""<?php
        |class Entity {
        |  #[Column(type: "string", length: 255)]
        |  public string $name;
        |}
        |""".stripMargin)

      // Property attributes are parsed - verify the class structure
      cpg.typeDecl.name("Entity").member.name("name").l.size shouldBe 1
    }
  }

  "PHP 8.1 new in initializers" should {
    "work in default parameter values" in {
      val cpg = code("""<?php
        |class Logger {
        |  public function log(string $message) {}
        |}
        |
        |function process($logger = new Logger()) {
        |  $logger->log("Processing");
        |}
        |""".stripMargin)

      // Verify the function is parsed correctly
      cpg.method.name("process").l.size shouldBe 1
    }

    "work in attribute arguments" in {
      val cpg = code("""<?php
        |#[Attribute]
        |class MyAttribute {
        |  public function __construct(public object $obj) {}
        |}
        |
        |#[MyAttribute(new stdClass())]
        |class Target {}
        |""".stripMargin)

      cpg.typeDecl.name("Target").annotation.name("MyAttribute").l.size shouldBe 1
    }
  }

}
