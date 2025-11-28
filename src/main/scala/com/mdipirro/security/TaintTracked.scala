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


class TaintTracked[P <: TaintLevel, +A] private(computeValue: () => A):
  import TaintTracked.Sanitised

  private lazy val value = computeValue()

  def open(using CanOpen[P]): A = value

  def map[B](f: A => B): TaintTracked[P, B] = new TaintTracked(() => f(value))

  def flatMap[P1 <: TaintLevel, B](f: A => TaintTracked[P1, B]): TaintTracked[TaintPropagation[P, P1], B] =
    val result = f(value)
    new TaintTracked(() => result.value)

  def foreach(f: A => Unit): Unit = f(value)

  def sanitise[E, B](s: A => Either[E, B]): Sanitised[E, B] =
    val result = s(value)
    result.map(r => new TaintTracked(() => r))

object TaintTracked:
  type Sanitised[E, A] = Either[E, TaintTracked[TaintLevel.Sanitised.type, A]]

  def apply[A](a: => A): TaintTracked[TaintLevel.Tainted.type, A] = new TaintTracked(() => a)

  // Creates a Pure TaintTracked value explicitly
  def unsafe[A](a: => A): TaintTracked[TaintLevel.Pure.type, A] = new TaintTracked(() => a)