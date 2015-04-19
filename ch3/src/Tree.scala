package Tree

sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left:Tree[A], right:Tree[A]) extends Tree[A]

object Tree {
    def size[A](t:Tree[A]): Int = {
        def go[A](b:Tree[A], c:Int): Int = b match {
            case Branch(l,r) => go(l, go(r, c+1))
            case Leaf(_) => c+1
        }
        go(t, 0)
    }

    def max(t:Tree[Int]): Int = {
        def go(b: Tree[Int], m:Int): Int = b match {
            case Branch(l, r) => go(l, go(r, m))
            case Leaf(v) => v max m
        }
        go(t, Int.MinValue)
    }

    def depth[A](t:Tree[A]): Int = {
        def go[A](b: Tree[A], d:Int, m:Int): Int = b match {
            case Branch(l, r) => go(l, d+1, go(r,d+1,m))
            case Leaf(_) => d max m
        }
        go(t, 0, 0)
    }

    def map[A,B](t:Tree[A])(f:A => B): Tree[B] = t match {
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
        case Leaf(v) => Leaf(f(v))
    }

}
