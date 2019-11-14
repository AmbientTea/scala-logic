package com.ambienttea.terms

import cats.data.StateT
import cats.implicits._
import cats.mtl.implicits._
import org.scalacheck.ScalacheckShapeless._
import org.scalatest.{Assertion, Matchers, WordSpec}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import Term._
import UnificationTest._

class UnificationTest extends WordSpec with Matchers with ScalaCheckDrivenPropertyChecks {
  type Env = Map[Symbol, Term]
  type OptionState[T] = StateT[Option, Env, T]

  def unify(a: Term, b: Term, env: Map[Symbol, Term] = Map.empty): Option[(Env, Term)] =
    Unification.unify[OptionState](a, b).run(env)

  "Unification" should {
    "unify any term with itself" in {
      forAll { (t: Term, e: Env) =>
        val vt = Reification.reify(t, e)
        unify(t, t, e) shouldBe Some((e, vt))
      }
    }

    "unify an unbound variable with any term" in {
      forAll { (v: Variable, t: Term, e: Env) =>
        // unifying with self does not change the environment
        val te = if (v == t) e else e + (v.name -> t)
        unify(v, t, e - v.name) shouldBe Some((te, t))
      }
    }

    "successfuly unify a cyclical term" in {
      val X = Variable('X)
      val fX = Functor('f, List(X))
      unify(X, fX) shouldBe Some((Map('X -> fX), fX))
    }

    "unify example terms" in {
      for ((t1, t2) <- exampleUnifiableTerms)
        withClue(s"$t1 = $t2") {
          unify(t1, t2) should not be empty
        }
    }
  }
}

object UnificationTest {

  import Term._
  import Term.Conversions._

  val exampleUnifiableTerms: List[(Term, Term)] = List(
    'a('X ?, 'Y ?, 'z('x, 'y)) -> 'a(1, 2, 'Z ?),
    'f('Y ?) -> 'f(1)
  )
}
