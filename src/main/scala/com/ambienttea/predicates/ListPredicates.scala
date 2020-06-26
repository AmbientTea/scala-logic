package com.ambienttea.predicates
import cats.mtl.MonadState
import com.ambienttea.terms._
import cats.MonoidK
import cats.Monad
import com.ambienttea.terms.Unification._
import com.ambienttea.terms.Term.Conversions._
import com.ambienttea.terms.Term._
import com.ambienttea.terms.Unification.Syntax._
import cats.implicits._
import Term._

class ListPredicates[F[_]](
    implicit S: MonadState[F, Map[Symbol, Term]],
    MO: MonoidK[F],
    M: Monad[F]
) {
  def length(list: Term, len: Term): F[Term] = {
    lazy val headCase = (list =? 'nil) *> (len =? 0)
    lazy val Tail = `_` // hacking around the lack of scope
    lazy val TailLength = `_`
    lazy val tailCase = for {
      _ <- list =? ':: (`_`, Tail)
      _ <- len =? 's (TailLength)
      len <- length(Tail, TailLength)
    } yield ('s (len): Term)

    headCase <+> tailCase
  }

  def member(list: Term, mem: Term): F[Term] = {
    lazy val Tail = 'Tail ??
    lazy val headCase = (list =? ':: (mem, `_`)) *> Reification.reify(mem)
    lazy val tailCase = for {
      _ <- list =? ':: (`_`, Tail)
      m <- member(Tail, mem)
    } yield m

    headCase <+> tailCase
  }

}
