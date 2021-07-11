
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

    // ex 3.25
    def size[A](tree: Tree[A]): Int = {
        tree match {
            case Leaf(_) => 1
            case Branch(left, right) => 1 + size(left) + size(right)
        }
    }

    // ex 3.26
    def maximum(tree: Tree[Int]): Int = {
        tree match {
            case Leaf(value) => value
            case Branch(left, right) => maximum(left) max maximum(right)
        }
    }

    // ex 3.27
    def depth[A](tree: Tree[A]): Int = {
        tree match {
            case Leaf(_) => 0
            case Branch(left, right) => 1 + depth(left) max depth(right)
        }
    }

    // ex 3.28
    def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = {
        tree match {
            case Leaf(value) => Leaf(f(value))
            case Branch(left, right) => Branch(map(left)(f), map(right)(f))
        }
    }

    // ex 3.29
    def fold[A,B](tree: Tree[A], z: A => B)(f: (B, B) => B): B =
        tree match {
            case Leaf(value) => z(value)
            case Branch(left, right) => f(fold(left, z)(f), fold(right, z)(f)) 
        }
    
    def sizeFold[A](tree: Tree[A]): Int ={
        fold(tree, (value: A) => 1)(1+_+_)
    }

    def maximumFold(tree: Tree[Int]): Int ={
        fold(tree, (value: Int) => value)((leftMax, rightMax) => leftMax max rightMax)
    }

    def depthFold[A](tree: Tree[A]): Int ={
        fold(tree, (value: A) => 0)((leftDepth, rightDepth) => 1 + (leftDepth max rightDepth))
    }

    def mapFold[A,B](tree: Tree[A])(f: A => B): Tree[B] ={
        fold(tree, (value: A) => Leaf(f(value)): Tree[B])((leftTree, rightTree) => Branch(leftTree, rightTree))
    }
}