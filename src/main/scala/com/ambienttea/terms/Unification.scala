package com.ambienttea.terms

import cats.implicits._
import cats.mtl._
import cats.{Monad, MonoidK}
import com.ambienttea.terms.Term._

import scala.language.postfixOps

object Unification {

  object Syntax {

    implicit class TermUnificationOps(term: Term) {
      def =?[F[_]](other: Term)(
        implicit S: MonadState[F, Map[Symbol, Term]],
        M: Monad[F],
        MK: MonoidK[F]
      ): F[Term] =
        unify(term, other)
    }

    implicit class SymbolUnificationOps(symbol: Symbol) {
      def =?[F[_]](other: Term)(
        implicit S: MonadState[F, Map[Symbol, Term]],
        M: Monad[F],
        MK: MonoidK[F]
      ): F[Term] =
        unify(Atom(symbol), other)
    }

  }

  def unify[F[_]](term1: Term, term2: Term)(
    implicit S: MonadState[F, Map[Symbol, Term]],
    M: Monad[F],
    MK: MonoidK[F]
  ): F[Term] = {
    //    println(s"?- $term1 = $term2")
    val result: F[Term] = (term1, term2) match {
      case (Atom(x), Atom(y)) if x == y => M.pure(term1)
      case (Number(x), Number(y)) if x == y => M.pure(term1)
      case (Variable(xn), Variable(yn)) => unifyVars(xn, yn)
      case (Variable(xn), term) => unifyVarWithTerm(xn, term)
      case (term, Variable(xn)) => unifyVarWithTerm(xn, term)
      case (Functor(f1, args1), Functor(f2, args2))
        if f1 == f2 && args1.size == args2.size => unifyFunctors(f1, args1, args2)
      case _ =>
        //        println(s"failed:  $term1 = $term2")
        MK.empty
    }

    result.map { v =>
      //      println(s"success: $term1 = $term2")
      v
    }
  }

  private def unifyVarWithTerm[F[_]](xn: Symbol, term: Term)(
    implicit S: MonadState[F, Map[Symbol, Term]], M: Monad[F], MK: MonoidK[F]
  ): F[Term] = {
    for {
      value <- S.inspect(_ get xn)
      result <- value match {
        case Some(vterm) => unify(term, vterm)
        case None =>
          S.modify(_ + (xn -> term)) *> M.pure(term)
      }
    } yield result
  }

  private def unifyVars[F[_]](xn: Symbol, yn: Symbol)(
    implicit S: MonadState[F, Map[Symbol, Term]],
    M: Monad[F],
    MK: MonoidK[F]
  ): F[Term] =
    for {
      env <- S.get
      result <- (env.get(xn), env.get(yn)) match {
        case (Some(x), Some(y)) => unify(x, y)
        case (None, None) if xn == yn => M.pure(Variable(yn): Term)
        case (None, None) =>
          val target: Term = Variable(yn)
          S.modify(_ + (xn -> target)) *> M.pure(target)
        case (x, y) =>
          val xy = (x orElse y).get
          S.modify(_ + (xn -> xy) + (yn -> xy)) *> M.pure(xy)
      }
    } yield result


  private def unifyFunctors[F[_]](symbol: Symbol, args1: List[Term], args2: List[Term])(
    implicit S: MonadState[F, Map[Symbol, Term]], M: Monad[F], MK: MonoidK[F]
  ): F[Term] = {
    val zipped = (args1, args2).zipped.toList
    for {
      res <- zipped.traverse {
        case (a, b) => unify(a, b)
      }
    } yield symbol(res: _*)
  }

}
