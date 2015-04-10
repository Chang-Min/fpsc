/**
 * Created by cmoh on 2015-04-09.
 */

package MyList

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def sum(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x, xs) => x * product(xs)
    }

    def prod[A](ds: List[A], r: A)(op: (A, A) => A): A = ds match {
        case Nil => r
        case Cons(x, xs) => op(x, prod(xs, r)(op))
    }

    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
        as match {
            case Nil => z
            case Cons(x, xs) => {
                //println(x)
                f(x, foldRight(xs, z)(f))
            }
        }

    def sum2(ns: List[Int]) = foldRight(ns, 0)(_ + _)

    def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

    def foldRightP[A, B](as: List[A], z: B)(f: (A, B) => B): B =
        as match {
            case Nil => z
            case Cons(x, xs) => {
                println(x)
                if (x == 0 || x == 0.0) f(x, foldRightP(Nil, z)(f))
                else f(x, foldRightP(xs, z)(f))
            }
        }
    def product3(ns: List[Double]) = foldRightP(ns, 1.0)(_ * _)
    def length[A](l: List[A]): Int = {
        def count(e: A, n: Int): Int = e match {
            case Nil => n
            case _ => n + 1
        }
        foldRight(l, 0)(count)
    }
    ////////////////////////////// foldLeft ///////////////////////////
    def foldLeft[A,B](as: List[A], z:B)(f: (B,A) => B): B = {
        @annotation.tailrec
        def go(l: List[A], r:B): B = l match {
            case Nil => r
            case Cons(x, xs) => go(xs, f(r, x))
        }
        go(as, z)
    }
    def sum4[A](ns: List[A], z: A)(f: (A,A)=>A) = foldLeft(ns, z)(f)
    def prd4[A](ns: List[A], z: A)(f: (A,A)=>A) = foldLeft(ns, z)(f)
    def len4[A](l: List[A]): Int = {
        def count(n:Int, e:A): Int = e match {
            case Nil => n
            case _ => n+1
        }
        foldLeft(l, 0)(count)
    }
    def reverse[A](l:List[A]): List[A] = {
        def build(l:List[A], r:List[A]): List[A] = l match {
            case Nil => r
            case Cons(x,xs) => build(xs, Cons(x, r))
        }
        build(l, Nil:List[A])
    }
    def rev2[A](l:List[A]): List[A] = foldLeft(l, Nil:List[A])((a,b) => Cons(b,a))
    def adder1(l:List[Int]): List[Int] =
}
