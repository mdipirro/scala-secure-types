package com.mdipirro.security


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
