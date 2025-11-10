package com.mdipirro.security

enum TaintLevel:
  case Pure, Sanitised, Tainted

trait CanOpen[P <: TaintLevel]

object CanOpen:
  given CanOpen[TaintLevel.Pure.type] with {}

  given CanOpen[TaintLevel.Sanitised.type] with {}

type TaintPropagation[P0 <: TaintLevel, P1 <: TaintLevel] <: TaintLevel = (P0, P1) match
  case (TaintLevel.Tainted.type, _) => TaintLevel.Tainted.type
  case (_, TaintLevel.Tainted.type) => TaintLevel.Tainted.type
  case (TaintLevel.Sanitised.type, _) => TaintLevel.Sanitised.type
  case (_, TaintLevel.Sanitised.type) => TaintLevel.Sanitised.type
  case (TaintLevel.Pure.type, TaintLevel.Pure.type) => TaintLevel.Pure.type


class TaintTracker[P <: TaintLevel, +A] private(computeValue: () => A):
  import TaintTracker.Sanitised

  private lazy val value = computeValue()

  def open(using CanOpen[P]): A = value

  def map[B](f: A => B): TaintTracker[P, B] = TaintTracker(f(value))

  def flatMap[P1 <: TaintLevel, B](f: A => TaintTracker[P1, B]): TaintTracker[TaintPropagation[P, P1], B] =
    val result = f(value)
    TaintTracker(result.value)

  def foreach(f: A => Unit): Unit = f(value)

  def sanitise[B, E](s: A => Either[E, B]): Sanitised[B, E] =
    val result = s(value)
    result.map(TaintTracker.apply)

object TaintTracker:
  type Sanitised[A, E] = Either[E, TaintTracker[TaintLevel.Sanitised.type, A]]

  def apply[P <: TaintLevel, A](a: => A): TaintTracker[P, A] = new TaintTracker(() => a)

@main def sanitise(): Unit =
  print("Enter a value: ")

  val tainted = TaintTracker[TaintLevel.Tainted.type, String](scala.io.StdIn.readLine())
  val sanitised = tainted sanitise { str =>
    str.toIntOption.toRight(s"$str is not a number")
  }

  val safeIncrement = TaintTracker[TaintLevel.Pure.type, Int](5)

  sanitised match
    case Left(error) => println(s"Sanitisation failed: $error")
    case Right(safeValue) =>
      val result = for {
        v <- safeValue
        i <- safeIncrement
      } yield v + i
      println(s"Sanitised computation result: ${result.open}")