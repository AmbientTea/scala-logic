package com.ambienttea.unification

import com.ambienttea.unification.Term.Conversions._
import com.ambienttea.unification.Term._
import com.ambienttea.unification.Unification.Environment
import scala.language.postfixOps
import cats.implicits._

object Main extends App {
  val env = Environment.empty
  val t1 = 'a('X ?, 'Y ?, 'z('x, 'y))
  val t2 = 'a(1, 2, 'Z ?)

  def printUnif(t1: Term, t2: Term) =
    println(s"$t1 = $t2 -> ${Unification.unify(t1, t2).run(env.variables)}")

  printUnif(1, 1)
  printUnif('a, 'a)
  printUnif('a, 'X ?)
  printUnif(1, 'X ?)
  printUnif('f(1), 'X ?)
  printUnif('f('Y ?), 'f(1))
  printUnif(t1, t2)

  printUnif('X?, 'f('X?))
  printUnif('X?, 'f('f('X?)))

}
