package reify.spec

import echo.spec.EchoSpec
import reify.Reified._
import reify.Token.{Compound, Identifier, Infix, Method, Primitive, TString}
import reify.{Formatter, Reified, Reify, Token}


class TokenSpec extends EchoSpec {
  "format" - {
    "Primitive" reifyEquals {
      Formatter.Unindented.format(abc)
    }

    "TString" - {
      "one liner" reifyEquals {
        Formatter.Unindented.format(TString("Here's a one line string."))
      }

      "with newlines" in {
        val threeQuotes = "\"\"\""

        assert(Formatter.Unindented.format(TString(
          """Here's a string.
            |
            |It has several lines.
            |
            |Here's the last line.""".stripMargin)
        ) ===
          s"""${threeQuotes}Here's a string.
            ~  |
            ~  |It has several lines.
            ~  |
            ~  |Here's the last line.${threeQuotes}.stripMargin""".stripMargin('~')
        )
      }
    }

    "Infix" - {
      "static" reifyEquals {
        Formatter.Unindented.format(Infix(Primitive("lhs"), " ~> ", Primitive("rhs")))
      }

      "dynamic" reifyEquals {
        Formatter.Unindented.format(Primitive("lhs") ~> Primitive("rhs"))
      }
    }

    "Method" - {
      "unindented" - {
        "no parameters" reifyEquals {
          Formatter.Unindented.format(Method(Primitive("target"), "method", Nil))
        }

        "with parameters" reifyEquals {
          Formatter.Unindented.format(
            Method(Primitive("target"), "method", List(Primitive("p1"), Primitive("p2")))
          )
        }
      }

      "indented" - {
        "no parameters" reifyEquals {
          Formatter.Indented(margin = 0, escape = false).format(Method(Primitive("target"), "method", Nil))
        }

        "with parameters" reifyEquals {
          Formatter.Indented(margin = 0, escape = false).format(
            Method(Primitive("target"), "method", List(Primitive("p1"), Primitive("p2")))
          )
        }
      }
    }

    "Compound" - {
      "unindented" reifyEquals {
        Formatter.Unindented.format(Compound("Foo", List(Primitive("3"))))
      }

      "indented" - {
        val one   = Primitive("1")
        val two   = Primitive("2")
        val three = Primitive("3")

        "non-nested" reifyEquals {
          Formatter.Indented(margin = 0, escape = false).format(list(one, two, three))
        }

        "nested" reifyEquals {
          Formatter.Indented(margin = 0, escape = false).format(list(one, set(two, three)))
        }
      }

      "wrapping" - {
        val ones   = Primitive("111")
        val twos   = Primitive("222")
        val threes = Primitive("333")
        val fours  = Primitive("444")

        "non-nested" reifyEquals {
          Formatter.Indented(margin = 22, escape = false).format(list(ones, twos, threes, fours))
        }

        "nested" reifyEquals {
          Formatter.Indented(margin = 24, escape = false).format(
            set(list(ones, twos), list(ones, twos, threes, fours))
          )
        }
      }

      "with infix" reifyEquals {
        Formatter.Unindented.format(Reify.reify(Map("key" -> 123)).tokenize)
      }

      "complex with infix" reifyEquals {
        Formatter.Unindented.format(Reify.reify(Reify.reify(Map("key" -> 123))).tokenize)
      }

      "complex with method" reifyEquals {
        Formatter.Unindented.format(Reify.reify(RMethod(RCaseObject("Foo"), "oof", Nil): Reified).tokenize)
      }

      "more complex with method" reifyEquals {
        Formatter.Unindented.format(Reify.reify(RMethod(RCaseObject("Foo"), "oof", List(RInt(123))): Reified).tokenize)
      }
    }
  }

  "parse" - {
    "primitive" in {
      assert(Token.parse("abc") === Right(abc))
    }

    "tstring" - {
      "double quoted" in {
        assert(Token.parse("\"abc\"") === Right(TString("abc")))
      }

      "triple quoted" in {
        assert(Token.parse("\"\"\"abc\"\"\"") === Right(TString("abc")))
      }
    }

    "compound" - {
      "no args" in {
        assert(Token.parse("abc()") === Right(Compound("abc", Nil)))
      }

      "one arg" in {
        assert(Token.parse("abc(def)") === Right(Compound("abc", List(Identifier("def")))))
      }

      "many args" in {
        assert(Token.parse("abc(def, ghi)") === Right(Compound("abc", List(Identifier("def"), ghi))))
      }

      "nested" in {
        assert(Token.parse("2") === Right(Primitive("2")))

        assert(Token.parse("subtract(2, 1)") === Right(
          Compound("subtract", List(Primitive("2"), Primitive("1")))
        ))
      }
    }

    "method" - {
      "one" in {
        assert(Token.parse("abc.def(ghi, jkl)") === Right(Method(abc, "def", List(ghi, Identifier("jkl")))))
      }

      "chain" in {
        assert(Token.parse("abc.def(ghi).jkl(mno).pqr(stu)") === Right(
          abc.method("def", ghi).method("jkl", Identifier("mno")).method("pqr", Identifier("stu"))
        ))
      }
    }

    "infix" - {
      "simple" in {
        assert(Token.parse("key := value") === Right(Infix(Primitive("key"), ":=", Primitive("value"))))
      }
    }
  }

  "kebab-case" - {
    "compound" in {
      assert(Token.Compound("SomeClass", Nil).kebabCase === Token.Compound("some-class", Nil))
    }

    "method" in {
      assert(Token.Method(
        Token.Compound("SomeClass", Nil), "someMethod", Nil
      ).kebabCase === Token.Method(
        Token.Compound("some-class", Nil), "some-method", Nil
      ))
    }
  }

  "camel-case" - {
    "compound" in {
      assert(Token.Compound("some-class", Nil).camelCase === Token.Compound("SomeClass", Nil))
    }

    "method" in {
      assert(Token.Method(
        Token.Compound("SomeClass", Nil), "some-method", Nil
      ).camelCase === Token.Method(
        Token.Compound("SomeClass", Nil), "someMethod", Nil
      ))
    }
  }

  private val abc: Identifier = Identifier("abc")
  private val ghi: Identifier = Identifier("ghi")

  private def list(tokens: Token*): Token = Compound("List", tokens.toList)
  private def set(tokens: Token*): Token = Compound("Set", tokens.toList)
}