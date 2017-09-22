package template

import org.scalatest.{FreeSpec, Matchers}

class ExprSpec extends FreeSpec with Matchers {

  "Literal" in {
    val expr = Expr(5)
    expr shouldEqual Literal(5)
    expr.inline shouldEqual 5
  }

  "Widen" in {
    val expr = Expr(5 : Any)
    expr shouldEqual Widen(Literal(5))
    expr.inline shouldEqual 5
  }

  "Match" in {
    val expr = Expr {
      "foo" match {
        case foo if foo startsWith "fo" => 10
        case "foo" => 20
        case foo => 30
      }
    }

    expr match {
      case Match(
        Literal("foo"), (
          CaseDef(Bind("foo", Ident("_")),Invoke(Ident("foo"),"startsWith",Literal("fo")),Literal(10)),
          CaseDef(Literal("foo"),Literal(true),Literal(20)),
          CaseDef(Bind("foo",Ident("_")),Literal(true),Literal(30))
          )
        ) =>
    }

    expr.inline shouldEqual 10
  }

  "Match with unapply" in {
    val expr = Expr {
      List(1, 2, 3) match {
        case List(1, 2, 3) => 10
        // TODO: this form does not currently work; the tree is very complicated for various reasons
        case 1 :: 2 :: 3 :: Nil => 20
        case _ => 30
      }
    }

    println(expr)
    expr.inline shouldEqual 10
  }

  "Let" in {
    val expr = Expr {
      val foo = 10
      foo
    }

    expr shouldEqual Let("foo", Literal(10), Ident("foo"))
    expr.inline shouldEqual 10
  }

  "Invoke" in {
    val expr = Expr {
      val foo = 10
      foo * 5
    }

    expr shouldEqual Let(
      "foo",
      Literal(10),
      Invoke(
        Ident("foo"),
        "*",
        Literal(5)
      )
    )

    expr.inline shouldEqual 50
  }

}
