package com.mdipirro.security

/**
 * Type class representing the lattice ordering relation between security levels
 */
trait LEQ[S1, S2]

/**
 * Proof of a security level
 */
case class Proof[S]()

/**
 * Type class instance for reflexive ordering (same level)
 */
given [S]: LEQ[S, S] with {} 