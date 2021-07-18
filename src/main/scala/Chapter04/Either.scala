package Chapter04

trait Either[+E, +A] {
  // ex 4.6
  def map[B](f: A => B): Either[E, B] =
    this match {
      case Left(value)  => Left(value)
      case Right(value) => Right(f(value))
    }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(value)  => Left(value)
      case Right(value) => f(value)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(value)  => b
      case Right(value) => Right(value)
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    //this.flatMap(aa => b.map(bb => f(aa,bb)))
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)

  // ex 4.7
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es match {
      case Nil          => Right(Nil)
      case head :: tail => head.flatMap(h => sequence(tail).map(h :: _))
    }
  
    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
      as match {
      case Nil          => Right(Nil)
      case head :: tail => f(head).flatMap(h => traverse(tail)(f).map(h :: _))
    }
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]
