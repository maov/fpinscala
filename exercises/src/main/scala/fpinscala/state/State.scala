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


//okey i'm lazy in theory to keep the function random one should keep generating untill != Int.minValue 
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val n = rng.nextInt
    if(n._1 < 0){
      if(n._1 == Int.MinValue)  (0, n._2)
      else  (n._1 * -1, n._2)
    }else
      n
  }
//lazy here again
  def double(rng: RNG): (Double, RNG) = {
    val n = nonNegativeInt(rng)
    val v = if(n._1 == Int.MaxValue) (Int.MaxValue -1).toDouble else n._1.toDouble
    var r =   v / (Int.MaxValue ).toDouble
    (r, n._2)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = ???

  def doubleInt(rng: RNG): ((Double,Int), RNG) = ???

  def double3(rng: RNG): ((Double,Double,Double), RNG) = ???

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = 
    {
    def subF(c: Int, l: List[Int], r: RNG) : (List[Int], RNG) =
      if(count > 0) {
        val t2 = r.nextInt 
        subF(count - 1, t2._1 :: l, t2._2)
    } else (l,r)
    subF(count, List(), rng)
  }



  def doubleMap : Rand[Double] = map(int)(n => {
    val v = if(n == Int.MaxValue) (Int.MaxValue -1).toDouble else n.toDouble
    v / (Int.MaxValue ).toDouble
  })

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng =>
  {
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng)
    (f(a,b), rng2)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = 
  fs match {
    // Rand[A], Rand[List[A]] => Rand[List[A]]
    // A, List[A] => List[A] , gives Rand[List[C]] in map2
    case h :: t => map2(h, sequence(t))((a,b) => a :: b)
    case _ => unit(List[A]())
  }


  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    val b = g(a)
    b(rng2)
  }

  def mapf[A,B](s: Rand[A])(f: A => B) : Rand[B] = flatMap(s)(x => unit(f(x)))

  def map2f[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = flatMap(ra)(
    a => flatMap(rb)(b => unit(f(a,b)))
  )
}
//type Rand[A] = State[RNG, A]
case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State(s => {
    val (a,s2) = run(s)
    (f(a),s2)
  })
//    ???
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a,b) ))
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(
    s => {
      val (a, s2) = run(s)
      f(a).run(s2)
      }
  )
    
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
