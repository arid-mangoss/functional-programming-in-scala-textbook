package Chapter05

import Chapter04.Option._

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

trait Stream[+A] {
  def toList: List[A] =
    this match {
      case Cons(h, t) => h() :: t().toList
      case Empty      => Nil
    }

  def headOption: Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => Some(h())
  }

  // ex 5.2
  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) =>
        if (n == 0) Empty else Cons(() => h(), () => t().take(n - 1))
      case Empty => Empty
    }

  def drop(n: Int): Stream[A] =
    this match {
      case Cons(h, t) => if (n == 0) this else t().drop(n - 1)
      case Empty      => Empty
    }

  // ex 5.3
  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) =>
        if (p(h())) Cons(() => h(), () => t().takeWhile(p)) else Empty
      case Empty => Empty
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _          => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  // ex 5.4
  def forAll(p: A => Boolean): Boolean =
    ! exists(a => !p(a))

}

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}

object run {
  def main(args: Array[String]): Unit = {
    val s = Stream.apply(1, 2, 3, 4, 5, 6, 7)
    println(s.drop(2).toList)
    println(s.take(3).toList)
    println(s.takeWhile(x => x < 3).toList)
  }
}
