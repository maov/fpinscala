package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means 
                                                    // that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  def forAll(p: A => Boolean) : Boolean =
    foldRight(true)( (a,b) => p(a) && b)

  def takeWhileFR(p: A => Boolean): Stream[A] = 
    foldRight(empty[A])( (a,b) => if(p(a)) cons(a, b) else empty[A])

    //reverse ???, how to find 
  def headOptionFR() : Option[A] = foldRight(none[A]) ( (a,b) =>  Some(a))

  def map[B](f: A => B) : Stream[B] = 
    foldRight(empty[B])( (a,b) => cons(f(a),b))

  def flatMap[B](f: A => Stream[B] ): Stream[B] = 
    foldRight(empty[B])( (a,b) => f(a) append b)

  def filter(f : A => Boolean) : Stream[A] = 
  foldRight(empty[A])( (a,b) => if(f(a)) cons(a, b) else b )

  def append[B>:A](s: => Stream[B]): Stream[B] = 
    foldRight(s)((h,t) => cons(h,t))

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList() : List[A] = { 
    def subToList(s : Stream[A]) : List[A] =  s match {
     case Cons(h,t) => h() :: subToList(t())
     case Empty => List()
    }
    subToList(this)
  }

  //TODO convert into using accumulator and recursive
  def take(n: Int): Stream[A] = {
    def subTake(i: Int, s: Stream[A]) : Stream[A] =
      s match {
        case Cons(h, t) if i > 0 => cons(h(), subTake(i - 1, t()))
        case _ => Empty

      }
      subTake(n, this)
    }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match{
    case Cons(h,t) if n > 0 => t().drop(n - 1)
    case Cons(h,t) if n == 0 => Cons(h,t)
    case Empty => Empty
  }
  

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) => if(p(h())) cons(h(), t().takeWhile(p)) else Empty
    case _ => Empty
  }

  def headOption: Option[A] = this match {
    case Cons(h,t) => Some(h())
    case _ => None
  }

  //  def unfoldM[A, S](z: S)(f: S => Option[(A, S)]): Stream[A]
  def mapUF[B](f: A => B) : Stream[B] = unfold(this)(s => s match {
      case Cons(h,t) => Some((f(h()),t()))
      case Empty => None
    }
    )
  
  def takeUF(n : Int) : Stream[A] = unfold((this, n))(s => s._1 match {
    case Cons(h,t) if s._2 > 0 => Some((h(), (t(), s._2 -1)))
    case _ => None
  })

  def takeWhileUF(p: A => Boolean) : Stream[A] = unfold(this)(
    s => s match {
      case Cons(h,t) => if(p(h())) Some((h(),t())) else None
      case Empty => None
    }
  )

  def zipAll[B](s2: Stream[B]) : Stream[(Option[A],Option[B])] = unfold((this,s2))(
    s => s._1 match {
      case Cons(h1,t1) => s._2 match {
        case Cons(h2,t2) => ???
        case Empty => Some((Some(h1()),None), (t1(), Empty))
      }
      case Empty => ???
    }
  )

  def htOption() : Option[(A,Stream[A])] = this match {
    case Cons(h,t) => Some(h(), t())
    case Empty => None
  }

  def zipWithUF[B](s2: Stream[B]) : Stream[(A,B)] = unfold((this,s2))(
    s => s._1.htOption().flatMap
    {case (h1,t1) => s._2.htOption().map
      {case (h2,t2) => ((h1,h2),(t1,t2)) }
    }
  )

  
  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  //zip and forAll (fold)
  def startsWith[B](s: Stream[B]): Boolean = ???
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
  def none[A] : Option[A] = None

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant(a : Int) : Stream[Int] = cons(a, constant(a))

  def from(n: Int) : Stream[Int] = cons(n, from(n + 1))
  def fib() : Stream[Int] = {
    def subfib(n: Int, m: Int) : Stream[Int] = cons(n + m, subfib(m, n + m))
    subfib(0,1)
  }


  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match
  {
    case Some((a,s)) => cons(a, unfold(s)(f))
    case None => Empty
  }

  def unfoldM[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map{ case (a,s) => cons(a,unfoldM(s)(f))}.getOrElse(empty[A])

    //write, fibs, from, ones, cons using unfold

  def onesUnfoldM = unfoldM(1)(x => Some((1,1)))
  def consUnfoldM(a: Int) = unfoldM(a)(x => Some((a,a)))
  def fromUnfoldM(n: Int) = unfoldM(n)(i => Some((i+1,i+1)))



}