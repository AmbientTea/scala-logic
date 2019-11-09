package com.ambienttea.unification

import Term._

object Unification {

  case class Environment(variables: Map[Symbol, Term] = Map.empty)

  object Environment {
    def empty: Environment = Environment()
  }

  def unify(env: Environment)(term1: Term, term2: Term): Option[(Environment, Term)] = {
    (term1, term2) match {
      case (Atom(x), Atom(y)) if x == y => Some((env, term1))
      case (Number(x), Number(y)) if x == y => Some((env, term1))
      case (Variable(xn), Variable(yn)) => unifyVars(env)(xn, yn)
      case (Variable(xn), term) => unifyVarWithTerm(env)(xn, term)
      case (term, Variable(xn)) => unifyVarWithTerm(env)(xn, term)
      case (Functor(f1, args1), Functor(f2, args2)) if f1 == f2 && args1.size == args2.size =>
        println(s">>> $f1: $args1, $args2")
        unifyFunctors(env)(f1, args1, args2)
      case _ => None
    }
  }

  private def unifyVarWithTerm(env: Environment)(xn: Symbol, term: Term): Option[(Environment, Term)] =
    env.variables.get(xn) match {
      case None =>
        val newVars = env.variables + (xn -> term)
        Some(env.copy(variables = newVars) -> term)
      case Some(vterm) => unify(env)(term, vterm)
    }

  private def unifyVars(env: Environment)(xn: Symbol, yn: Symbol): Option[(Environment, Term)] = {
    val v = env.variables
    (v.get(xn), v.get(yn)) match {
      case (Some(x), Some(y)) => unify(env)(x, y)
      case (x, y) =>
        for {
          xy <- x orElse y
          newVars = env.variables + (xn -> xy) + (yn -> xy)
          newEnv = env.copy(variables = newVars)
        } yield (newEnv, xy)
    }
  }

  private def unifyFunctors(env: Environment)(
    symbol: Symbol, args1: List[Term], args2: List[Term]
  ): Option[(Environment, Term)] = {
    val (newEnv: Option[Environment], args: List[Nothing]) =
      (args1, args2).zipped
        .foldLeft(Option(env) -> List[Term]()) {
          case ((env, stack), (t1, t2)) =>
            env.flatMap(unify(_)(t1, t2)) match {
              case None => (None, Nil)
              case Some((env, term)) => (Some(env), term :: stack)
            }
        }

    newEnv.map(_ -> symbol(args: _*))
  }

}
