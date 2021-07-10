import scala.util.control.TailCalls

object Chapter2 {


    def fib(n: Int): Int = {
        
        def loop(a: Int, b: Int, n: Int): Int = 
            if (n == 1)
                b 
            else
                loop(b, b + a, n - 1)
        loop(0, 1, n)
    }

    def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
        def loop(n: Int): Boolean = 
            if (n >= as.length) 
                true
            else if (ordered(as(n), as(n-1)))
                loop(n+1)
            else
                false
        if (as.length >= 2)
            loop(1)
        else
            true
    }

    def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
        a => b => f(a, b)
    }

    def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
        (a, b) => f(a)(b)
    }

    
    def compose[A,B,C](f: B => C, g: A => B): A => C = {
        a => f(g(a))
    }
    
    def main(args: Array[String]): Unit = {
        println(fib(5))
    }

    val x = List(1,2,3,4,5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) =>x+y
        case Cons(h, t) => h + List.sum(t)
        case _ => 101
    }
}