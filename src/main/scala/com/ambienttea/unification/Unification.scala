package com.ambienttea.unification

import Term._
import cats.implicits._
import cats.data._
import language.postfixOps

object Unification {
  type UnificationState[T] = StateT[Option, Map[Symbol, Term], T]

  case class Environment(variables: Map[Symbol, Term] = Map.empty)

  object Environment {
    def empty: Environment = Environment()
  }

  def unify(term1: Term, term2: Term): StateT[Option, Map[Symbol, Term], Term] = {
    (term1, term2) match {
      case (Atom(x), Atom(y)) if x == y => StateT.pure(term1)
      case (Number(x), Number(y)) if x == y => StateT.pure(term1)
      case (Variable(xn), Variable(yn)) => unifyVars(xn, yn)
      case (Variable(xn), term) => unifyVarWithTerm(xn, term)
      case (term, Variable(xn)) => unifyVarWithTerm(xn, term)
      case (Functor(f1, args1), Functor(f2, args2))
        if f1 == f2 && args1.size == args2.size => unifyFunctors(f1, args1, args2)
      case _ => StateT.liftF(None)
    }
  }

  private def unifyVarWithTerm(xn: Symbol, term: Term): UnificationState[Term] = {
    for {
      env <- StateT.get[Option, Map[Symbol, Term]]
      result <- env.get(xn) match {
        case Some(vterm) => unify(term, vterm)
        case None => StateT.set[Option, Map[Symbol, Term]](env + (xn -> term)) *> StateT.pure(term)
      }
    } yield result
  }

  private def unifyVars(xn: Symbol, yn: Symbol): UnificationState[Term] =
    for {
      env <- StateT.get[Option, Map[Symbol, Term]]
      result <- (env.get(xn), env.get(yn)) match {
        case (Some(x), Some(y)) => unify(x, y)
        case (x, y) =>
          for {
            xy <- StateT.liftF[Option, Map[Symbol, Term], Term](x orElse y)
            _ <- StateT.modify[Option, Map[Symbol, Term]](_ + (xn -> xy) + (yn -> xy))
          } yield xy
      }
    } yield result

  private def unifyFunctors(
                             symbol: Symbol, args1: List[Term], args2: List[Term]
                           ): UnificationState[Term] = {
    for {
      res <- (args1, args2).zipped.toList.traverse(unify _ tupled)
    } yield symbol(res: _*)
  }

}
