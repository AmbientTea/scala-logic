package com.ambienttea.unification

import scala.language.implicitConversions

sealed trait Term

case class Variable(name: Symbol) extends Term {
  override def toString: String = name.name.toLowerCase.capitalize
}

case class Atom(value: Symbol) extends Term {
  override def toString: String = value.name.toLowerCase
}

case class Number(value: Int) extends Term {
  override def toString: String = value.toString
}

case class Functor(op: Symbol, arguments: List[Term]) extends Term {
  override def toString: String = s"$op(${arguments.map(_.toString).mkString(", ")})"
}

object Term {

  implicit class SymbolOps(symbol: Symbol) {
    def ? : Variable = Variable(symbol)

    def apply(args: Term*): Functor = Functor(symbol, args.toList)

    def a: Atom = Atom(symbol)
  }

  object Conversions {
    implicit def number(n: Int): Number = Number(n)

    implicit def atom(s: Symbol): Atom = Atom(s)
  }

}


