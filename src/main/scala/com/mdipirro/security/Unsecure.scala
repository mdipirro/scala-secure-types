package com.mdipirro.security

/**
 * Type alias for validation functions
 */
type ValidationFunctions[A, B] = List[A => Option[B]]

/**
 * Unsecure value wrapper with validation functions
 */
case class Unsecure[A, B](validationFunctions: ValidationFunctions[A, B], value: A)

object Unsecure:
  /**
   * Validate an encapsulated value. If at least one error occurs, this
   * function returns a list with every occurred error (Right). Otherwise the
   * actual value is returned (Left).
   */
  def validate[A, B](unsecure: Unsecure[A, B]): Either[A, List[B]] =
    val errors = unsecure.validationFunctions
      .flatMap(f => f(unsecure.value))
    if errors.isEmpty then Left(unsecure.value)
    else Right(errors)

  /**
   * Maps a function on the encapsulated value. The function must not change the
   * type since the constraints are defined on the `A` type.
   */
  def umap[A, B](unsecure: Unsecure[A, B])(f: A => A): Unsecure[A, B] =
    Unsecure(unsecure.validationFunctions, f(unsecure.value))

  /**
   * Factory method to create an unsecure value
   */
  def upure[A, B](value: A, validationFunctions: ValidationFunctions[A, B]): Unsecure[A, B] =
    Unsecure(validationFunctions, value) 