package Chapter05

import Chapter04.Option._
import Stream._

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
    !exists(a => !p(a))

  // ex 5.5
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else Empty)

  // ex 5.6
  def headOptionViaFoldRight: Option[A] =
    foldRight(None: Option[A])((h, t) => Some(h))

  // ex 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)

  def append[B >: A](bs: => Stream[B]): Stream[B] =
    foldRight(bs)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

  // ex 5.13
  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this)(s =>
      s match {
        case Cons(h, t) => Some((f(h()), t()))
        case Empty      => None
      }
    )

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n))(s =>
      s match {
        case (Cons(h, t), 1)  => Some((h(), (empty, 0)))
        case (Cons(h, t), nn) => Some((h(), (t(), nn - 1)))
        case _                => None
      }
    )

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    unfold(this)(s =>
      s match {
        case Cons(h, t) => if (f(h())) Some((h(), t())) else None
        case _          => None
      }
    )

  def zipWithViaUnfold[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, bs))(s =>
      s match {
        case (Cons(a, ax), Cons(b, bx)) => Some((f(a(), b()), (ax(), bx())))
        case _                          => None
      }
    )

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2))(s =>
      s match {
        case (Cons(h, t), Cons(h2, t2)) =>
          Some((Some(h()), Some(h2())) -> (t(), t2()))
        case (Cons(h, t), Empty)   => Some((Some(h()), None) -> (t(), empty))
        case (Empty, Cons(h2, t2)) => Some((None, Some(h2())) -> (empty, t2()))
        case _                     => None
      }
    )

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

  // ex 5.8
  def constant[A](a: A): Stream[A] = {
    Cons(() => a, () => constant(a))
    // lazy val c: Stream[A] = Stream.cons(a, c)
    // c
  }

  // ex 5.9
  def from(n: Int): Stream[Int] =
    Cons(() => n, () => from(n + 1))

  // ex 5.10
  def fibs(): Stream[Int] = {
    def innerFib(state: (Int, Int)): Stream[Int] =
      cons(state._1, innerFib(state._2, state._1 + state._2))
    innerFib(0, 1)
  }

  // ex 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None         => empty
    }

  // ex 5.12
  def fibsViaUnfold(): Stream[Int] =
    unfold((0, 1))((s: (Int, Int)) => Some((s._1 + s._2, (s._2, s._1 + s._2))))

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(s => Some((s, s + 1)))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(s => Some(s, s))

  def onesViaUnfold(): Stream[Int] =
    constantViaUnfold(1)

}

object run {
  def main(args: Array[String]): Unit = {
    val s = Stream.apply(1, 2, 3, 4, 5, 6, 7)
    println(s.drop(2).toList)
    println(s.take(3).toList)
    println(s.takeWhile(x => x < 3).toList)
    println(s.takeWhileViaFoldRight(x => x < 3).toList)
    println(s.headOptionViaFoldRight)
    println(s.map(_ + 1).toList)
    println(s.filter(_ < 4).toList)

    val ones = Stream.constant(1)
    println(ones.take(10).toList)

    val fromOne = Stream.from(1)
    println(fromOne.take(10).toList)

    val fibStream = Stream.fibs()
    println(fibStream.take(10).toList)

    val fibStreamViaUnfold = Stream.fibsViaUnfold()
    println(fibStreamViaUnfold.take(10).toList)

    println(fromViaUnfold(10).take(10).toList)
    println(constantViaUnfold(10).take(10).toList)
  }
}
