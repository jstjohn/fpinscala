package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i,rng2) = rng.nextInt
    (if (i < 0) -(i + 1) else i, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i,r) = nonNegativeInt(rng)
    ((i.toDouble / (Int.MaxValue.toDouble+1)), r)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i,r) = rng.nextInt
    val (d,r2) = double(r)
    ((i,d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i,d),r) = intDouble(rng)
    ((d,i),r)
  }
  

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d0,r0) = double(rng)
    val (d1,r1) = double(r0)
    val (d2,r2) = double(r1)
    ((d0,d1,d2),r2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(rem: Int, l:List[Int], r:RNG):(List[Int],RNG) = {
      if(rem <= 0) (l, r)
      else{
        val (i,r2) = r.nextInt
        go(rem-1, i::l, r2)
      } 
    }
    go(count, Nil:List[Int], rng)
  }
  

  def doubleViaMap: Rand[Double] = 
    map(nonNegativeInt)(_.toDouble / (Int.MaxValue.toDouble+1))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rng =>
    val (a, r1) = ra(rng)
    val (b, r2) = rb(r1)
    (f(a,b), r2)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(Nil:List[A]))((a,b) => map2(a,b)(_::_))
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (a,r) = f(rng)
    g(a)(r)
  }
  

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt){
    i => {
      val m = i%n
      if(i+(n-1)-m > 0) unit(m)
      else nonNegativeLessThan(n)
    }
  }

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = 
    flatMap(s){a => unit(f(a))}

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    flatMap(ra){a => flatMap(rb){ b => unit(f(a,b)) }}
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State { s:S => {
      val (a,s2) = this.run(s)
      (f(a),s2)
    }
  }
    
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  
  def flatMap[B](f: A => State[S, B]): State[S, B] = sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = ???

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}