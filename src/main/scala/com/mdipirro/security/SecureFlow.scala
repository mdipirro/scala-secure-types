package com.mdipirro.security

/**
 * SecureFlow: the Identity monad tagged with a proposition allowing to access its value
 */
case class SecureFlow[S, A](value: A):
  def map[B](f: A => B): SecureFlow[S, B] = SecureFlow(f(value))
  def flatMap[B](f: A => SecureFlow[S, B]): SecureFlow[S, B] = f(value)
  def <*>[B](f: SecureFlow[S, A => B]): SecureFlow[S, B] = map(f.value)

  def open[S2](using LEQ[S, S2], Proof[S2]): A = value

  def up[S2](using LEQ[S, S2]): SecureFlow[S2, A] = SecureFlow(value)

type Hatch[S, A, B] = SecureFlow[S, A => B]
type HatchPrime[S, L, A, B] = SecureFlow[L, SecureFlow[S, A => B]]

object SecureFlow:
  def apply[S, A](a: A): SecureFlow[S, A] = new SecureFlow(a)

  /**
   * Returns the second (more restrictive) Hatch version
   */
  def makeHatch[S, L, A, B](f: A => B): HatchPrime[S, L, A, B] = SecureFlow(SecureFlow(f))

  /**
   * Declassification
   */
  private def unsafeCoerceLevels[S, S2, A](sf: SecureFlow[S, A])(using LEQ[S2, S]): SecureFlow[S2, A] = SecureFlow(sf.value)

  def declassifyWith[S, K, S2, A, B](hatch: Hatch[K, A, B], sf: SecureFlow[S, A])(
    using LEQ[S, K], LEQ[S2, S]
  ): SecureFlow[S2, B] =
    unsafeCoerceLevels(sf.flatMap(x => SecureFlow(hatch.value(x))))

  def declassifyWithPrime[S, K, L, S2, A, B](hatch: HatchPrime[K, L, A, B], sf: SecureFlow[S, A])(
    using LEQ[S, K], LEQ[S2, S], LEQ[L, S2]
  ): SecureFlow[S2, B] = declassifyWith(hatch.value, sf)