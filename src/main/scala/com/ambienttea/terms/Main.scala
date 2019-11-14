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

  type BacktrackingComputation[T] = StateT[LazyList, Map[Symbol, Term], T]

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

  def length[F[_]](list: Term, len: Term)(implicit S: MonadState[F, Map[Symbol, Term]],
                                          M: Monad[F],
                                          MO: MonoidK[F]): F[Term] = {
    lazy val headCase = (list =? 'nil) *> (len =? 0)
    lazy val Tail = `_` // hacking around the lack of scope
    lazy val TailLength = `_`
    lazy val tailCase = for {
      _ <- list =? '::(`_`, Tail)
      len <- length(Tail, TailLength)
    } yield ('s(len): Term)

    headCase <+> tailCase
  }


  val termList = '::(0, '::(1, '::(2, 'nil)))

  def computation3[F[_] : Monad](implicit S: MonadState[F, Map[Symbol, Term]],
                                 MO: MonoidK[F]): F[Term] = {
    length(termList, `_`)
  }

  println(s"list length = ${computation3[BacktrackingComputation].runA(Map.empty).toList}")

  def member[F[_]](list: Term, mem: Term)(implicit S: MonadState[F, Map[Symbol, Term]],
                                          M: Monad[F],
                                          MO: MonoidK[F]): F[Term] = {
    lazy val Tail = 'Tail ??
    lazy val headCase = list =? '::(mem, `_`)
    lazy val tailCase = for {
      _ <- list =? '::(`_`, Tail)
      m <- member(Tail, mem)
    } yield m

     headCase <+> tailCase
  }

  def computation4[F[_] : Monad](implicit S: MonadState[F, Map[Symbol, Term]],
                                 MO: MonoidK[F]): F[Term] = {
    member(termList, 'Member ?) *> ('Member.? =? 'Member.?)
  }

  val  members = computation4[BacktrackingComputation].runA(Map.empty).toList
  println(s"list members = $members")
}
