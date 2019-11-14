package com.ambienttea.terms

object Reification {
  def reify(term: Term, env: Map[Symbol, Term]): Term =
    reify(env)(term)

  def reify(env: Map[Symbol, Term])(term: Term): Term =
    term match {
      case v@Variable(name) =>
        env.get(name).filter(_ != v).map(reify(env)).getOrElse(v)
      case Functor(op, arguments) =>
        Functor(op, arguments.map(reify(env)))
      case other => other
    }
}
