package Chapter03


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

    // ex 3.11
    def sumLeft(ints: List[Int]): Int = 
        foldLeft(ints, 0)(_ + _)

    def productLeft(ds: List[Double]): Double = 
        foldLeft(ds, 1.0)(_ * _)
    
    def lengthLeft[A](as: List[A]): Int = 
        foldLeft(as, 0)((acc:Int, _) => 1+acc)

    // ex 3.12
    def reverseLeft[A](as: List[A]): List[A] = {
        foldLeft(as, List[A]())((acc,head) => Cons(head,acc))
    }

    def reverseRight[A](as: List[A]): List[A] = {
        foldLeft(as, List[A]())((head,acc) => Cons(acc,head))
    }

    def reverse[A](as: List[A]): List[A] = {
        def loop(l:List[A], acc:List[A]): List[A] =
            l match {
                case Nil => acc
                case Cons(head, tail) => loop(tail, Cons(head, acc))
            }
        loop(as, List())
    }

    // ex 3.13
    def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B =
        foldLeft(as, z)((a,b) => f(b,a))
    

    def foldLeftViaFoldRight[A,B](as: List[A], z: B)(f: (B, A) => B): B = 
        foldRight(as, z)((b, a) => f(a,b))
    
    // ex 3.14
    def append[A](as: List[A], bs: List[A]): List[A] =
        // you have to reverse `as` if you use foldLeft
        foldRight(as, bs)((a,acc) => Cons(a,acc))

    // ex 3.15
    def concat[A](lists: List[List[A]]): List[A] =
        foldLeft(lists, List[A]())((acc,as) => append(acc,as))
    
    // ex 3.16
    def addOne(as: List[Int]): List[Int] = 
        as match {
            case Nil => Nil
            case Cons(x, xs) => Cons(x+1, addOne(xs))
        }
        
    // ex 3.17
    def toString(as: List[Double]): List[String] =
        as match {
            case Nil => Nil
            case Cons(x, xs) => Cons(x.toString, toString(xs))
        }

    // ex 3.18
    def map[A,B](as: List[A])(f: A => B): List[B] = 
        as match {
            case Nil => Nil
            case Cons(x, xs) => Cons(f(x), map(xs)(f))
        }

    // ex 3.19
    def filter[A](as: List[A])(f: A => Boolean): List[A] = 
        as match {
            case Nil => Nil
            case Cons(x, xs) => if (f(x)) Cons(x, filter(xs)(f)) else filter(xs)(f)
        }

    // ex 3.20
    def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = 
        as match {
            case Nil => Nil
            case Cons(x, xs) => append(f(x), flatMap(xs)(f))
        }

    // ex 3.21
    def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
        flatMap(as)(a => if (f(a)) List(a) else List())

    // ex 3.22
    def zipAdd(as: List[Int], bs: List[Int]): List[Int] = 
        (as, bs) match {
            case (Nil, Nil) => Nil
            case (Nil, _) => Nil
            case (_, Nil) => Nil
            case (Cons(x, xs), Cons(y, ys)) => Cons(x+y, zipAdd(xs,ys))
        }
    
    // ex 3.23
    def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A,B) => C): List[C] =
        (as, bs) match {
            case (Nil, Nil) => Nil
            case (Nil, _) => Nil
            case (_, Nil) => Nil
            case (Cons(x, xs), Cons(y, ys)) => Cons(f(x,y), zipWith(xs,ys)(f))
        }

    // ex 3.24
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
        def startsWith[A](supInner: List[A], subInner: List[A]): Boolean = 
            (supInner, subInner) match {
                case (Cons(x, _), Cons(y, Nil)) => if (x == y) true else false
                case (Cons(x, xs), Cons(y, ys)) => if (x == y) startsWith(xs, ys) else false
                case (_, _) => false
            }
        sup match {
            case Nil => false
            case Cons(x, xs) => if (startsWith(sup, sub)) true else hasSubsequence(xs, sub)
        }
    }
}
