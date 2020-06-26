package com.ambienttea.terms

import scala.language.implicitConversions
import scala.util.Random

sealed trait Term

case class Variable(name: Symbol) extends Term {
  override def toString: String = s"${name.name.toLowerCase.capitalize}?"
}

case class Atom(value: Symbol) extends Term {
  override def toString: String = s"`${value.name.toLowerCase}`"
}

case class Number(value: Int) extends Term {
  override def toString: String = value.toString
}

case class Functor(op: Symbol, arguments: List[Term]) extends Term {
  override def toString: String = s"$op(${arguments.map(_.toString).mkString(", ")})"
}

object Term {

  def `_` = Variable(Symbol("_" + Random.nextString(6)))

  implicit class SymbolOps(symbol: Symbol) {
    def ? : Variable = Variable(symbol)
    def ?? : Variable = Variable(Symbol(s"_${symbol}${Random.nextString(2)}"))

    def apply(args: Term*): Functor = Functor(symbol, args.toList)

    def a: Atom = Atom(symbol)
  }

  object Conversions {
    implicit def number(n: Int): Number = Number(n)

    implicit def atom(s: Symbol): Atom = Atom(s)
  }

}


