package com.mdipirro.security

import com.mdipirro.security.WithPropagation.SecureComputation
import com.mdipirro.security.WithPropagation.SecureComputation.Sanitised

object WithPurity:

  trait MustBePure[M]

  case class Pure()

  case class Tainted()

  given MustBePure[Pure] with {}

  /**
   * Secure computation wrapper
   */
  case class SecureComputation[M, +A] private(value: A):

    def open(using MustBePure[M]): A = value

    def map[M2, B](f: A => B)(using MustBePure[M], MustBePure[M2]): SecureComputation[M2, B] = SecureComputation(f(value))

    def flatMap[M2, B](f: A => SecureComputation[M2, B])(using MustBePure[M], MustBePure[M2]): SecureComputation[M2, B] =
      f(value)

    def sapp[M2, B](f: SecureComputation[M2, A => B])(using MustBePure[M], MustBePure[M2]): SecureComputation[M2, B] =
      map(f.value)

    def foreach(f: A => Unit)(using MustBePure[M]): Unit = f(value)

  private object SecureComputation:
    def apply[M, A](a: A): SecureComputation[M, A] = new SecureComputation(a)

object WithPropagation:
  enum Purity:
    case Pure, Tainted, Sanitised

  // Typeclass to allow open only for Pure and Sanitised
  trait CanOpen[P <: Purity]

  object CanOpen:
    given CanOpen[Purity.Pure.type] with {}

    given CanOpen[Purity.Sanitised.type] with {}

  type Propagate[P0 <: Purity, P1 <: Purity] <: Purity = (P0, P1) match
    case (Purity.Tainted.type, _) => Purity.Tainted.type
    case (_, Purity.Tainted.type) => Purity.Tainted.type
    case (Purity.Sanitised.type, _) => Purity.Sanitised.type
    case (_, Purity.Sanitised.type) => Purity.Sanitised.type
    case (Purity.Pure.type, Purity.Pure.type) => Purity.Pure.type


  case class SecureComputation[P <: Purity, +A] private(value: A):
    def open(using CanOpen[P]): A = value

    def map[B](f: A => B): SecureComputation[P, B] = SecureComputation(f(value))

    def flatMap[P1 <: Purity, B](f: A => SecureComputation[P1, B]): SecureComputation[Propagate[P, P1], B] =
      val result = f(value)
      SecureComputation(result.value)

    def foreach(f: A => Unit): Unit = f(value)

    def sanitise[B, E](s: A => Either[E, B]): Sanitised[B, E] =
      val result = s(value)
      result.map(SecureComputation.apply)

  object SecureComputation:
    type Sanitised[A, E] = Either[E, SecureComputation[Purity.Sanitised.type, A]]

    def apply[P <: Purity, A](a: A): SecureComputation[P, A] = new SecureComputation(a)

@main def sanitise(): Unit =
  val untrusted = "10"
  val tainted = SecureComputation[WithPropagation.Purity.Tainted.type, String](untrusted)
  val sanitised = tainted sanitise { str =>
    str.toIntOption.toRight(s"$str is not a number")
  }

  val safeIncrement = SecureComputation[WithPropagation.Purity.Pure.type, Int](5)

  sanitised match
    case Left(error) => println(s"Sanitisation failed: $error")
    case Right(safeValue) =>
      val result = for {
        v <- tainted
        i <- safeIncrement
      } yield v + i
      println(s"Sanitised computation result: ${result.open}")
