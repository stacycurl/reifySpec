package reify.spec

import echo.spec.EchoSpec
import reify.Reified.{RBoolean, RCaseClass, RCaseObject, REither, RInfix, RInt, RList, RMap, RMethod, ROption, RPrimitive, RString}
import reify.Token.{Compound, Identifier, Infix, Method, Primitive, TString}
import reify.{Reified, Reify, Token}


class ReifiedSpec extends EchoSpec {
  "reify" - {
    "RBoolean" in {
      (RBoolean(true): Reified) <=> RBoolean(true)
    }

    "RInt" in {
      (RInt(123): Reified) <=> RInt(123)
    }

    "RString" in {
      (RString("Foo"): Reified) <=> RString("Foo")
    }

    "RPrimitive" in {
      (RPrimitive("Foo"): Reified) <=> RPrimitive("Foo")
    }

    "ROption" - {
      "none" in {
        (ROption(None): Reified) <=> ROption(None)
      }

      "some" in {
        (ROption(Some(RInt(123))): Reified) <=> ROption(Some(RInt(123)))
      }
    }

    "REither" - {
      "left" in {
        (REither(Left(RPrimitive("left"))): Reified) <=> REither(Left(RPrimitive("left")))
      }

      "right" in {
        (REither(Right(RInt(123))): Reified) <=> REither(Right(RInt(123)))
      }
    }

    "RList" - {
      "empty" in {
        (RList(Nil): Reified) <=> RList(Nil)
      }

      "nonEmpty" in {
        (RList(List(RInt(123), RInt(456))): Reified) <=> RList(List(RInt(123), RInt(456)))
      }
    }

    "RMap" - {
      "empty" in {
        (RMap(Map.empty): Reified) <=> RMap(Map.empty)
      }

      "nonEmpty" in {
        (RMap(Map(RString("key") -> RInt(123))): Reified) <=> RMap(Map(RString("key") -> RInt(123)))
      }
    }

    "RCaseClass" in {
      (RCaseClass("Foo", 3): Reified) <=> RCaseClass("Foo", 3)
    }

    "RCaseObject" in {
      (RCaseObject("Foo"): Reified) <=> RCaseObject("Foo")
    }

    "Infix" in {
      (RInfix(123, " + ", 456): Reified) <=> RInfix(123, " + ", 456)
    }

    "Method" - {
      "0 parameters" in {
        (RMethod(RCaseObject("Foo"), "oof", Nil): Reified) <=> RMethod(RCaseObject("Foo"), "oof", Nil)
      }

      "n parameters" in {
        (RMethod(RCaseObject("Foo"), "oof", List(RInt(123))): Reified) <=> RMethod(RCaseObject("Foo"), "oof", List(RInt(123)))
      }
    }
  }

  "reify.reflect" - {
    def roundtripEquals[A: Reify](a: A): Unit = {
      val reified: Reified     = Reify.reify(a)
      val reflected: Option[A] = Reify.reflect(reified)

      assert(reflected === Some(a))
    }

    "RBoolean" in {
      roundtripEquals(RBoolean(true): Reified)
    }

    "RInt" in {
      roundtripEquals(RInt(123): Reified)
    }

    "RString" in {
      roundtripEquals(RString("Foo"): Reified)
    }

    "RPrimitive" in {
      roundtripEquals(RPrimitive("Foo"): Reified)
    }

    "ROption" - {
      "none" in {
        roundtripEquals(ROption(None): Reified)
      }

      "some" in {
        roundtripEquals(ROption(Some(RInt(123))): Reified)
      }
    }

    "REither" - {
      "left" in {
        roundtripEquals(REither(Left(RPrimitive("left"))): Reified)
      }

      "right" in {
        roundtripEquals(REither(Right(RInt(123))): Reified)
      }
    }

    "RList" - {
      "empty" in {
        roundtripEquals(RList(Nil): Reified)
      }

      "nonEmpty" in {
        roundtripEquals(RList(List(RInt(123), RInt(456))): Reified)
      }
    }

    "RMap" - {
      "empty" in {
        roundtripEquals(RMap(Map.empty): Reified)
      }

      "nonEmpty" in {
        roundtripEquals(RMap(Map(RString("key") -> RInt(123))): Reified)
      }
    }

    "RCaseClass" in {
      roundtripEquals(RCaseClass("Foo", 3, "four"): Reified)
    }

    "RCaseObject" in {
      roundtripEquals(RCaseObject("Foo"): Reified)
    }

    "Infix" in {
      roundtripEquals(RInfix(123, " + ", 456): Reified)
    }

    "Method" - {
      "0 parameters" in {
        roundtripEquals(RMethod(RCaseObject("Foo"), "oof", Nil): Reified)
      }

      "n parameters" in {
        roundtripEquals(RMethod(RCaseObject("Foo"), "oof", List(RInt(123))): Reified)
      }
    }
  }

  "tokenize" - {
    "RBoolean" in {
      RBoolean(true).tokenize <=> Primitive("true")
    }

    "RInt" in {
      RInt(123).tokenize <=> Primitive("123")
    }

    "RString" - {
      "double quotes" in {
        RString("Foo").tokenize <=> TString("Foo")
      }

      "triple quotes" in {
        RString("""Foo""").tokenize <=> TString("Foo")
      }

      "triple quotes containing a quote" in {
        RString("""Foo " ooF""").tokenize <=> TString("Foo \" ooF")
      }
    }

    "RPrimitive" - {
      "double quotes" in {
        RPrimitive("Foo").tokenize <=> Primitive("Foo")
      }

      "triple quotes containing a quote" in {
        RPrimitive("""Foo " ooF""").tokenize <=> Primitive("""Foo " ooF""")
      }
    }

    "ROption" - {
      "none" in {
        ROption(None).tokenize <=> Primitive("None")
      }

      "some" in {
        ROption(Some(RInt(123))).tokenize <=> Compound("Some", List(Primitive("123")))
      }
    }

    "RList" - {
      "empty" in {
        RList(Nil).tokenize <=> Compound("List", List())
      }

      "nonEmpty" in {
        RList(List(RInt(123), RInt(456))).tokenize <=> Compound("List", List(Primitive("123"), Primitive("456")))
      }
    }

    "RMap" - {
      "empty" in {
        RMap(Map.empty).tokenize <=> Compound("Map", List())
      }

      "nonEmpty" in {
        RMap(Map(RString("key") -> RInt(123))).tokenize <=> Token.compound(
          "Map",
          List(Token.infix(Token.primitive("key"), " -> ", Token.primitive("123")))
        )
      }
    }

    "RCaseClass" in {
      RCaseClass("Foo", 3).tokenize <=> Compound("Foo", List(Primitive("3")))
    }

    "RCaseObject" in {
      RCaseObject("Foo").tokenize <=> Identifier("Foo")
    }

    "Infix" in {
      // Token.infix(Token.primitive("123"), " + ", Token.primitive("456"))
      RInfix(123, " + ", 456).tokenize <=> Infix(Primitive("123"), " + ", Primitive("456"))
    }

    "Method" - {
      "0 parameters" in {
        RMethod(RCaseObject("Foo"), "oof", Nil).tokenize <=> Method(Identifier("Foo"), "oof", List())
      }

      "n parameters" in {
        RMethod(RCaseObject("Foo"), "oof", List(RInt(123))).tokenize <=> Method(Identifier("Foo"), "oof", List(Primitive("123")))
      }
    }
  }

  "parse" - {
    "boolean" in {
      Reified.parse(Primitive("true"))  <=> Some(RBoolean(true))
      Reified.parse(Primitive("false")) <=> Some(RBoolean(false))
    }

    "int" in {
      Reified.parse(Primitive("123")) <=> Some(RInt(123))
    }

    "string" in {
      Reified.parse(TString("Foo")) <=> Some(RString("Foo"))
      // TODO: What about strings containing double quotes ?
    }

    "option" - {
      "none" in {
        Reified.parse(Primitive("None")) <=> Some(ROption(None))
      }

      "some" in {
        Reified.parse(Compound("Some", List(Primitive("123")))) <=> Some(ROption(Some(RInt(123))))
      }
    }

    "list" - {
      "empty" in {
        Reified.parse(Compound("List", List())) <=> Some(RList(Nil))
      }

      "nonEmpty" in {
        Reified.parse(Compound("List", List(Primitive("123"), Primitive("456")))) <=> Some(RList(List(RInt(123), RInt(456))))
      }
    }

    "RMap" - {
      "empty" in {
        Reified.parse(Compound("Map", List())) <=> Some(RMap(Map.empty))
      }

      "nonEmpty" in {
        Reified.parse(Token.compound(
          "Map",
          List(Token.infix(Token.primitive("key"), " -> ", Token.primitive("123")))
        )) <=> Some(RMap(Map(RPrimitive("key") -> RInt(123))))
      }
    }

    "RCaseClass" in {
      Reified.parse(Compound("Foo", List(Primitive("3")))) <=> Some(RCaseClass("Foo", List(RInt(3))))
    }

    "Infix" in {
      Reified.parse(Infix(Primitive("123"), " + ", Primitive("456"))) <=> Some(RInfix(123, " + ", 456))
    }

    "Method" - {
      "0 parameters" in {
        Reified.parse(Method(Primitive("Foo"), "oof", List())) <=> Some(RMethod(RPrimitive("Foo"), "oof", Nil))
      }

      "n parameters" in {
        Reified.parse(Method(Primitive("Foo"), "oof", List(Primitive("123")))) <=> Some(
          RMethod(RPrimitive("Foo"), "oof", List(RInt(123)))
        )
      }
    }
  }
}
