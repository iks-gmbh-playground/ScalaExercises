package com.iks.codewars.isomorphism

import scala.sys.error

object Isomorphism {
  /**
   * The type [[Nothing]] has no value.
   * So it is impossible to construct an instance of it.
   * In this solution, wherever a situation arises where
   * for types to check, you need a function that takes a [[Nothing]],
   * you can use [[absurd]].
   */
  def absurd[R](n: Nothing): R = ???

  // so, when are two type, `A` and `B`, considered equal?
  // a definition might be, it is possible to go from `A` to `B`,
  // and from `B` to `A`.
  // Going a roundway trip should leave you the same value.
  // Unfortunately it is virtually impossible to test this in Scala.
  // This is called Isomorphism.

  type ISO[A, B] = (A => B, B => A)

  // given ISO a b, we can go from a to b
  def substL[A, B]: ISO[A, B] => A => B = _._1

  // and vice versa
  def substR[A, B]: ISO[A, B] => B => A = _._2

  // There can be more than one ISO a b
  def isoBool: ISO[Boolean, Boolean] = (identity, identity)

  def isoBoolNot: ISO[Boolean, Boolean] = (!_, !_)

  // isomorphism is reflexive
  def refl[A]: ISO[A, A] = (identity, identity)

  // isomorphism is symmetric
  def symm[A, B]: ISO[A, B] => ISO[B, A] = {
    case (ab, ba) => (ba, ab)
  }

  // isomorphism is transitive
  def trans[A, B, C]: (ISO[A, B], ISO[B, C]) => ISO[A, C] = {
    case ((ab, ba), (bc, cb)) =>
      (ab.andThen(bc), cb.andThen(ba))
  }

  // We can combine isomorphism:
  def isoTuple[A, B, C, D]: (ISO[A, B], ISO[C, D]) => ISO[(A, C), (B, D)] = {
    case ((ab, ba), (cd, dc)) =>
      (ac => (ab(ac._1), cd(ac._2)), bd => (ba(bd._1), dc(bd._2)))
  }

  def isoList[A, B]: ISO[A, B] => ISO[List[A], List[B]] = {
    case (ab, ba) =>
      (_ map ab, _ map ba)
  }

  def isoOption[A, B]: ISO[A, B] => ISO[Option[A], Option[B]] = {
    case (ab, ba) =>
      (_ map ab, _ map ba)
  }

  def isoEither[A, B, C, D]: (ISO[A, B], ISO[C, D]) => ISO[Either[A, C], Either[B, D]] = {
    case ((ab, ba), (cd, dc)) =>
      ( {
        case Left(a) => Left(ab(a))
        case Right(c) => Right(cd(c))
      }, {
        case Left(b) => Left(ba(b))
        case Right(d) => Right(dc(d))
      })
  }

  def isoFunc[A, B, C, D]: (ISO[A, B], ISO[C, D]) => ISO[A => C, B => D] = {
    case ((ab, ba), (cd, dc)) =>
      (ac => ba.andThen(ac).andThen(cd), bd => ab.andThen(bd).andThen(dc))
  }

  // Going another way is hard (and is generally impossible)
  def isoUnOption[A, B]: ISO[Option[A], Option[B]] => ISO[A, B] = {
    case (oab, oba) =>
      (a => oab(Option(a)) match {
        case Some(b) => b
        case None => oab(None).get
      }, b => oba(Option(b)) match {
        case Some(a) => a
        case None => oba(None).get
      })
  }

  // Remember, for all valid ISO, converting and converting back
  // Is the same as the original value.
  // You need this to prove some case are impossible.

  // We cannot have
  // isoUnEither[A, B, C, D]: (ISO[Either[A, B], Either[C, D]], ISO[A, C]) => ISO[B, D]
  // Note that we have
  def isoEU: ISO[Either[List[Unit], Unit], Either[List[Unit], Nothing]] =
    ( {
      case Left(xs) => Left(() :: xs)
      case _ => Left(Nil)
    }, {
      case Left(Nil) => Right()
      case Left(_ :: xs) => Left(xs)
      case Right(void) => absurd(void)
    })

  // where Unit, has 1 value, (the value is also called Unit), and Nothing has 0 values.
  // If we have isoUnEither,
  // We have ISO[Unit, Nothing] by calling isoUnEither isoEU
  // That is impossible, since we can get a Nothing by substL on ISO[Unit, Nothing]
  // So it is impossible to have isoUnEither

  // And we have isomorphism on isomorphism!
  def isoSymm[A, B]: ISO[ISO[A, B], ISO[B, A]] =
    ( {
      case (ab, ba) => (ba, ab)
    }, {
      case (ba, ab) => (ab, ba)
    })

}
