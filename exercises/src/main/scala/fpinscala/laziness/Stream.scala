package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = foldRight(Nil:List[A])((a, b) => a::b)

  def take(n: Int): Stream[A] = 
    if(n <= 0)
      empty
    else
      this match {
      case Cons(h,t) => cons(h(), t().take(n-1))
      case _ => this
    }

  def takeViaUnfold(n: Int): Stream[A] = 
    unfold((n,this))(s => s match {
      case (0, _) => None
      case (m, Cons(h,t)) => Some(h(), (m-1, t()))
      case (_, empty) => None
    })
  
  @annotation.tailrec
  final def drop(n: Int): Stream[A] =
    if(n < 1)
      this
    else
      this match{
        case Cons(h, t) => t().drop(n - 1)
        case empty => empty
      }

  def takeWhile(p: A => Boolean): Stream[A] = 
    foldRight(empty:Stream[A])((a,b)=> if(p(a)) cons(a,b) else empty)
  
  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = 
    unfold(this)(s => s match{
      case Cons(h,t) => if(p(h())) Some(h(),t()) else None
      case empty => None
    })

  def forAll(p: A => Boolean): Boolean = 
    foldRight(true)((a, b) => p(a) && b)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    takeWhile(p)

  def headOption: Option[A] = 
    foldRight(None:Option[A])((a,b)=> Some(a))
  
  def map[B](f: A => B): Stream[B] = 
    foldRight(empty:Stream[B])((h,t) => cons(f(h),t))

  def mapViaUnfold[B](f: A => B): Stream[B] = 
    unfold(this)(a => a match{
      case Cons(h,t) => Some( f(h()), t() )
      case empty => None
    })

  def filter(p: A => Boolean): Stream[A] = 
    foldRight(empty:Stream[A])((h,t) => if(p(h)) cons(h,t) else t)

  def append[B>:A](other: Stream[B]): Stream[B] = 
    foldRight(other)((h,t) => cons(h,t))

  def flatMap[B](f: A => Stream[B]): Stream[B] = 
    foldRight(empty:Stream[B])((h,t) => f(h).append(t))

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = 
    unfold((this,s2))(s => s match {
      case (Cons(h1,t1),Cons(h2,t2)) => Some(f(h1(),h2()), (t1(),t2()))
      case _ => None
    })

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = 
    unfold((this,s2))(s => s match {
      case (Cons(h1,t1), Cons(h2,t2)) => Some((Some(h1()),Some(h2())),(t1(), t2()))
      case (Cons(h1,t1), empty) => Some((Some(h1()),None),(t1(), empty))
      case (empty, Cons(h2,t2)) => Some((None,Some(h2())),(empty, t2()))
      case _ => None
    })
  
  
  def startsWith[B](s: Stream[B]): Boolean = 
    this.zipAll(s).forAll(s=> s match {
      case (Some(a),Some(b)) => a == b
      case (Some(a),None) => true
      case _ => false
    })

  def tails: Stream[Stream[A]] = unfold(this)(s => s match {
    case Cons(h,t) => Some((s, t()))
    case Empty => None
  }) append Stream(empty)

  def scanRight[B](s: B)(f: (A, B) => B): Stream[B] = sys.error("todo")
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = 
    cons(a, constant(a))

  def from(n: Int): Stream[Int] = 
    cons(n, from(n+1))
  
  def fibs_helper(i:Int, j:Int):Stream[Int] = cons(i, fibs_helper(j, i+j))

  lazy val fibs: Stream[Int] =
    fibs_helper(0,1)
    
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case Some((a,s)) => cons(a, unfold(s)(f))
      case None => empty
    }

  lazy val fibsViaUnfold: Stream[Int] = 
    unfold((0,1))(s => Some(s._1, (s._2, s._1+s._2)))

  def fromViaUnfold(n: Int): Stream[Int] = 
    unfold(n)(m => Some(m, m+1))

  def constantViaUnfold[A](a: A): Stream[A] = 
    unfold(a)(Some(_,a))

  lazy val onesViaUnfold: Stream[Int] = unfold(1)(Some(_,1))
}