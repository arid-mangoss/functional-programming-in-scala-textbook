trait Option[+A] {
  // ex 4.1

  def map[B](f: A => B): Option[B] =
    this match {
      case None      => None
      case Some(get) => Some(f(get))
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B =
    this match {
      case None      => default
      case Some(get) => get
    }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(a => Some(a)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    flatMap((a: A) => if (f(a)) Some(a) else None)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // ex 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  // ex 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aa => b.map(bb => f(aa, bb)))
  // or alternatively
  /*(a, b) match {
      case (None, _)                => None
      case (_, None)                => None
      case (Some(getA), Some(getB)) => Some(f(getA, getB))
    }*/

  // ex 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil          => Some(Nil)
      case head :: tail => head.flatMap(h => sequence(tail).map(h :: _))
    }

  // ex 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil          => Some(Nil)
      case head :: tail => f(head).flatMap(h => traverse(tail)(f).map(h :: _))
    }

  // ex 4.6
  // sequence in terms of traverse
  def sequenceUsingTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)
}
