package com.ambienttea.terms

import cats.data.StateT
import cats.implicits._
import cats.mtl.MonadState
import cats.mtl.implicits._
import cats.{Monad, MonoidK}
import com.ambienttea.terms.Term.Conversions._
import com.ambienttea.terms.Term._
import com.ambienttea.terms.Unification.Syntax._

import scala.language.postfixOps
import scala.util.Random

object Main extends App {

  implicit class SymbolOps2(symbol: Symbol) {
    def ?? : Variable = Variable(Symbol(s"_${symbol}${Random.nextString(2)}"))
  }

  type Env = Map[Symbol, Term]

  class predicates[F[_]](implicit S: MonadState[F, Env], MO: MonoidK[F], M: Monad[F]) {
    def length(list: Term, len: Term): F[Term] = {
      lazy val headCase = (list =? 'nil) *> (len =? 0)
      lazy val Tail = `_` // hacking around the lack of scope
      lazy val TailLength = `_`
      lazy val tailCase = for {
        _ <- list =? '::(`_`, Tail)
        _ <- len =? 's(TailLength)
        len <- length(Tail, TailLength)
      } yield ('s(len): Term)

      headCase <+> tailCase
    }

    def member(list: Term, mem: Term): F[Term] = {
      lazy val Tail = 'Tail ??
      lazy val headCase = (list =? '::(mem, `_`)) *> Reification.reify(mem)
      lazy val tailCase = for {
        _ <- list =? '::(`_`, Tail)
        m <- member(Tail, mem)
      } yield m

      headCase <+> tailCase
    }

  }

  type BacktrackingComputation[T] = StateT[LazyList, Map[Symbol, Term], T]

  object predicates extends predicates[BacktrackingComputation]

  import predicates.{length, member}

  /*

      def computation1[F[_] : Monad](implicit S: MonadState[F, Map[Symbol, Term]],
                                     MO: MonoidK[F]): F[Term] =
        for {
          _ <- 'term =? 'X.?
          _ <- 'f('X ?, 'term2) =? 'f('Y ?, 'Z ?)
          xy <- 'X.? =? 'Y.?
        } yield xy

      println(s"no backtracking: x = y = ${computation1[BacktrackingComputation].runA(Map.empty).toList}")

      def computation2[F[_] : Monad](implicit S: MonadState[F, Map[Symbol, Term]],
                                     MO: MonoidK[F]): F[Term] =
        for {
          _ <- ('term =? 'X.?) <+> ('term2 =? 'X.?)
          _ <- 'f('X ?, 'term2) =? 'f('Y ?, 'Z ?)
          xy <- 'X.? =? 'Y.?
        } yield xy

      println(s"backtracking: x = y = ${computation2[BacktrackingComputation].runA(Map.empty).toList}")
    */

  val termList = '::(0, '::(1, '::(2, 'nil)))

  def computation3 = length(termList, `_`)

  println(s"list length = ${computation3.runA(Map.empty).toList}")

  def computation4 = member(termList, 'Member ?)

  val members = computation4.runA(Map.empty).toList
  println(s"list members = $members")

  def computation5 =
    for {
      _ <- length('List ?, 's('s('s(0))))
      list <- Reification.reify[BacktrackingComputation]('List ?)
    } yield list

  println(s"3 element list = ${computation5.runA(Map.empty).toList}")
}
