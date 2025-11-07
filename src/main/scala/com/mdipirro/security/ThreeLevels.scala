package com.mdipirro.security

/**
 * Security levels in the system
 */
enum SecurityLevel:
  case Low, Medium, High

/**
 * Type class instances for security level ordering
 */
object SecurityLevel:
  given LEQ[SecurityLevel.Low.type, SecurityLevel.Low.type] with {}
  given LEQ[SecurityLevel.Low.type, SecurityLevel.Medium.type] with {}
  given LEQ[SecurityLevel.Low.type, SecurityLevel.High.type] with {}
  given LEQ[SecurityLevel.Medium.type, SecurityLevel.Medium.type] with {}
  given LEQ[SecurityLevel.Medium.type, SecurityLevel.High.type] with {}
  given LEQ[SecurityLevel.High.type, SecurityLevel.High.type] with {}

  /**
   * Proofs for security levels
   */
  val low: Proof[SecurityLevel.Low.type] = Proof()
  val medium: Proof[SecurityLevel.Medium.type] = Proof()
  private val high: Proof[SecurityLevel.High.type] = Proof() 