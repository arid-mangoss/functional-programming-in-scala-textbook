

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def sum(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(x,xs) => x + sum(xs)
    }
    def product(ds: List[Double]): Double = ds match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x,xs) => x * product(xs)
    }
    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

    // ex 3.2
    def tail[A](l: List[A]): List[A] = l match {
        case Cons(head, tail) => tail
    } 

    // ex 3.3
    def setHead[A](l: List[A], elem: A): List[A] = l match {
        case Cons(head, tail) => Cons(elem, tail)
        case Nil => Cons(elem, Nil)
    } 

    // ex 3.4
    def drop[A](l: List[A], n: Int): List[A] = {
        if (n==0) l
        else l match {
                case Nil => Nil
                case Cons(head, tail) => drop(tail, n-1)
            }
    }
    
    // ex 3.5
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
        l match {
            case Cons(head, tail) => {
                if (f(head)) dropWhile(tail, f)
                else tail
            }
        }
    }
    
    // ex 3.6
    def init[A](l: List[A]): List[A] = {
        l match {
            case Cons(last, Nil) => Nil
            case Cons(head, tail) => Cons(head, init(tail))
        }
    }
    
    // ex 3.7
    // No - foldRight only halts when there is a Nil (ie. end of list)

    // ex 3.8
    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
        as match {
          case Nil => z
          case Cons(x, xs) => f(x, foldRight(xs, z)(f))
        }
    }

    // ex 3.9
    def length[A](as: List[A]): Int = 
        foldRight(as, 0)((_, b:Int) => 1+b)
    
    // ex 3.10
    def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
        as match {
            case Nil => z
            case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
        }
    }

    // ex 3.12
    def reverse[A](as: List[A]): List[A] = {
        as match {
            case Nil => Nil
            case Cons(last, Nil) => last
            case Cons(head, tail) => Cons(reverse(tail), head)
        }
    }
}
