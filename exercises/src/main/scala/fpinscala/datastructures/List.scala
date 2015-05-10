package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Cons(h,t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = Cons(h, tail(l))
  
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = if(n <= 0) l else l match {
    case Cons(h,t) => drop(t,n-1)
    case Nil => Nil 
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h,t) => if(f(h)) dropWhile(t,f) else l
    case Nil => Nil
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(h, Nil) => Nil
    case Cons(h,t) => Cons(h,init(t))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((a:A, b:Int) => b+1)
  
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sumViaFoldLeft(nums: List[Int]): Int = foldLeft(nums,0)(_+_)

  def productViaFoldLeft(nums: List[Double]): Double = foldLeft(nums,1.0)(_*_)

  def lengthViaFoldLeft(l: List[_]): Int = foldLeft(l,0)((b,a)=>b+1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil:List[A])((b:List[A], a:A) => Cons(a,b))

  def appendViaFoldRight[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1,l2)((a:A, b:List[A]) => Cons(a,b))

  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] = foldLeft(reverse(a1),a2)((b:List[A], a:A) => Cons(a,b) )
  
  def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = 
    foldLeft(reverse(as),z)((b,a)=>f(a,b))
  
  def concat[A](l: List[List[A]]): List[A] = foldRightViaFoldLeft(l, Nil:List[A])(appendViaFoldLeft(_,_))

  def add1(nums: List[Int]): List[Int] = map(nums)(_+1)

  def doubleToString(l: List[Double]): List[String] = map(l)(_.toString)

  def map[A,B](l: List[A])(f: A => B): List[B] = 
    foldRightViaFoldLeft(l, Nil:List[B])((a,b)=>Cons(f(a), b))

  def filter[A](l: List[A])(f: A => Boolean): List[A] = 
    foldRightViaFoldLeft(l, Nil:List[A])((a,b)=>if(f(a)) Cons(a,b) else b)

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = 
    foldRightViaFoldLeft(l, Nil:List[B])((a,b)=>appendViaFoldLeft(f(a),b))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = 
    flatMap(l)(a => if(f(a)) List(a) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = zipWith(a,b)(_+_)

  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a,b) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(ah, at), Cons(bh, bt)) => Cons(f(ah,bh), zipWith(at,bt)(f))
  }
  
  @annotation.tailrec
  def subIsContained[A](l:List[A], sub:List[A]):Boolean = (l,sub) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(ah, at), Cons(bh, bt)) => if(ah == bh) subIsContained(at,bt) else false
  }
  
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = if(subIsContained(l,sub)) true else l match{
    case Cons(h,t) => hasSubsequence(t,sub)
    case Nil => false
  }
}
