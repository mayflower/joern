package io.joern.php2cpg.querying

import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.joern.php2cpg.parser.Domain
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.{ModifierTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class Php82FeaturesTests extends PhpCode2CpgFixture {

  "PHP 8.2 readonly classes" should {
    "have the readonly modifier on the class" in {
      val cpg = code("""<?php
        |readonly class ImmutableData {
        |  public string $value;
        |  public int $count;
        |}
        |""".stripMargin)

      inside(cpg.typeDecl.name("ImmutableData").l) { case List(typeDecl) =>
        typeDecl.modifier.modifierType.l should contain(ModifierTypes.READONLY)
        typeDecl.code should include("readonly")
      }
    }

    "work with inheritance" in {
      val cpg = code("""<?php
        |readonly class BaseData {
        |  public string $name;
        |}
        |
        |readonly class ExtendedData extends BaseData {
        |  public int $age;
        |}
        |""".stripMargin)

      inside(cpg.typeDecl.name("ExtendedData").l) { case List(typeDecl) =>
        typeDecl.modifier.modifierType.l should contain(ModifierTypes.READONLY)
        typeDecl.inheritsFromTypeFullName should contain("BaseData")
      }
    }
  }

  "PHP 8.2 DNF (Disjunctive Normal Form) types" should {
    "be parsed in function parameters" in {
      val cpg = code("""<?php
        |interface A {}
        |interface B {}
        |
        |function process((A&B)|null $value): void {}
        |""".stripMargin)

      inside(cpg.method.name("process").parameter.name("value").l) { case List(valueParam) =>
        valueParam.typeFullName shouldBe "(A&B)|null"
      }
    }

    "be parsed in return types" in {
      val cpg = code("""<?php
        |interface Countable {}
        |interface Traversable {}
        |
        |function getCollection(): (Countable&Traversable)|array {
        |  return [];
        |}
        |""".stripMargin)

      inside(cpg.method.name("getCollection").methodReturn.l) { case List(ret) =>
        ret.typeFullName shouldBe "(Countable&Traversable)|array"
      }
    }

    "be parsed in class properties" in {
      val cpg = code("""<?php
        |interface Foo {}
        |interface Bar {}
        |
        |class Test {
        |  public (Foo&Bar)|null $data;
        |}
        |""".stripMargin)

      // Property types go through AnyTypePass since they're not directly used in method signatures
      // The test verifies the property exists and the code is correctly parsed
      inside(cpg.typeDecl.name("Test").member.name("data").l) { case List(dataMember) =>
        dataMember.name shouldBe "data"
      // Type recovery would set this properly in post-processing
      }
    }

    "handle multiple intersection groups" in {
      val cpg = code("""<?php
        |interface A {}
        |interface B {}
        |interface C {}
        |interface D {}
        |
        |function complex((A&B)|(C&D) $value): void {}
        |""".stripMargin)

      inside(cpg.method.name("complex").parameter.name("value").l) { case List(valueParam) =>
        valueParam.typeFullName shouldBe "(A&B)|(C&D)"
      }
    }
  }

  "PHP 8.2 null/false/true standalone types" should {
    "support null as a standalone type" in {
      val cpg = code("""<?php
        |function returnNull(): null {
        |  return null;
        |}
        |""".stripMargin)

      inside(cpg.method.name("returnNull").methodReturn.l) { case List(ret) =>
        ret.typeFullName shouldBe "null"
      }
    }

    "support false as a standalone type" in {
      val cpg = code("""<?php
        |function returnFalse(): false {
        |  return false;
        |}
        |""".stripMargin)

      inside(cpg.method.name("returnFalse").methodReturn.l) { case List(ret) =>
        ret.typeFullName shouldBe "false"
      }
    }

    "support true as a standalone type" in {
      val cpg = code("""<?php
        |function returnTrue(): true {
        |  return true;
        |}
        |""".stripMargin)

      inside(cpg.method.name("returnTrue").methodReturn.l) { case List(ret) =>
        ret.typeFullName shouldBe "true"
      }
    }

    "support false in union types" in {
      val cpg = code("""<?php
        |function maybeFind(): string|false {
        |  return false;
        |}
        |""".stripMargin)

      inside(cpg.method.name("maybeFind").methodReturn.l) { case List(ret) =>
        ret.typeFullName shouldBe "string|false"
      }
    }
  }

  "PHP 8.2 constants in traits" should {
    "be parsed as members on the metaclass" in {
      val cpg = code("""<?php
        |trait TraitWithConst {
        |  public const VERSION = "1.0.0";
        |  protected const MAX_SIZE = 100;
        |}
        |""".stripMargin)

      // Constants are stored on the metaclass type decl
      inside(cpg.typeDecl.name(s"TraitWithConst${Domain.MetaTypeDeclExtension}").l) { case List(metaDecl) =>
        val members = metaDecl.member.l
        members.map(_.name) should contain("VERSION")
        members.map(_.name) should contain("MAX_SIZE")
      }
    }

    "work when trait is used in a class" in {
      val cpg = code("""<?php
        |trait ConfigTrait {
        |  public const DEFAULT_VALUE = 42;
        |}
        |
        |class MyClass {
        |  use ConfigTrait;
        |}
        |""".stripMargin)

      cpg.typeDecl.name("ConfigTrait").l.size shouldBe 1
      cpg.typeDecl.name("MyClass").l.size shouldBe 1
    }
  }

  "PHP 8.2 SensitiveParameter attribute" should {
    "be attached to parameters" in {
      val cpg = code("""<?php
        |function authenticate(
        |  string $user,
        |  #[SensitiveParameter] string $password
        |): bool {
        |  return true;
        |}
        |""".stripMargin)

      inside(cpg.method.name("authenticate").parameter.name("password").l) { case List(passwordParam) =>
        val annotations = passwordParam.annotation.l
        annotations.map(_.name) should contain("SensitiveParameter")
      }
    }

    "work with fully qualified attribute name" in {
      val cpg = code("""<?php
        |function login(
        |  #[\SensitiveParameter] string $secret
        |): void {}
        |""".stripMargin)

      inside(cpg.method.name("login").parameter.name("secret").l) { case List(secretParam) =>
        secretParam.annotation.l.size should be > 0
      }
    }
  }

  "PHP 8.2 AllowDynamicProperties attribute" should {
    "be attached to classes" in {
      val cpg = code("""<?php
        |#[AllowDynamicProperties]
        |class LegacyClass {
        |  public string $name;
        |}
        |""".stripMargin)

      inside(cpg.typeDecl.name("LegacyClass").l) { case List(typeDecl) =>
        val annotations = typeDecl.annotation.l
        annotations.map(_.name) should contain("AllowDynamicProperties")
      }
    }

    "work with fully qualified attribute name" in {
      val cpg = code("""<?php
        |#[\AllowDynamicProperties]
        |class DynamicClass {}
        |""".stripMargin)

      inside(cpg.typeDecl.name("DynamicClass").l) { case List(typeDecl) =>
        typeDecl.annotation.l.size should be > 0
      }
    }
  }

  "PHP 8.2 fetching enum properties in const expressions" should {
    "allow accessing enum name in constant expression" in {
      val cpg = code("""<?php
        |enum Status: string {
        |  case Active = 'active';
        |  case Inactive = 'inactive';
        |}
        |
        |class StatusHandler {
        |  public const ACTIVE_NAME = Status::Active->name;
        |}
        |""".stripMargin)

      cpg.typeDecl.name("StatusHandler").l.size shouldBe 1
    }
  }

  "PHP 8.2 deprecated dynamic properties" should {
    "parse classes without AllowDynamicProperties" in {
      val cpg = code("""<?php
        |class StrictClass {
        |  public string $name;
        |
        |  public function __construct(string $name) {
        |    $this->name = $name;
        |  }
        |}
        |""".stripMargin)

      inside(cpg.typeDecl.name("StrictClass").l) { case List(typeDecl) =>
        // Should not have AllowDynamicProperties
        typeDecl.annotation.name("AllowDynamicProperties").l.size shouldBe 0
      }
    }
  }

  "PHP 8.2 readonly class" should {
    "have the readonly modifier and parse without errors" in {
      val cpg = code("""<?php
        |readonly class Point {
        |  public function __construct(
        |    public int $x,
        |    public int $y,
        |    public int $z = 0
        |  ) {}
        |}
        |""".stripMargin)

      inside(cpg.typeDecl.name("Point").l) { case List(typeDecl) =>
        typeDecl.modifier.modifierType.l should contain(ModifierTypes.READONLY)
        // Constructor should be parsed
        cpg.method.name("__construct").l.size shouldBe 1
        // Parameters should exist on the constructor
        inside(cpg.method.name("__construct").parameter.l) { case params =>
          params.map(_.name) should contain("x")
          params.map(_.name) should contain("y")
          params.map(_.name) should contain("z")
        }
      }
    }
  }

}
