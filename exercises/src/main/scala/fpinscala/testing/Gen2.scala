package fpinscala.testing2

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

// trait Prop  {
//   self => 
//   def check : Boolean = ???
//   def &&(p: Prop): Prop = new Prop {
//     override def check = self.check && p.check
//   }
// }


//Add label to result if it exists og return a tuble with result/label
case class Prop(run: (MaxSize, TestCases, RNG) => Result, label: Option[String] = None){ self =>
  def &&(p: Prop) :Prop = Prop( (max, a, rng) => { val b = self.run(max, a, rng); if(b != Passed) b else p.run(max, a, rng) })
  def ||(p: Prop) :Prop = Prop( (max, a, rng) => { val b = self.run(max, a, rng); if(b == Passed) b else p.run(max, a, rng) })
}


sealed trait Result {
  def isFalsified : Boolean
}

case object Passed  extends Result {
  def isFalsified = false
}

case class Falsified(failure: FailedCase, successes: SucessCount) extends Result {
  def isFalsified = true
}

object Prop {
  type FailedCase = String
  type SucessCount = Int
  type TestCases = Int
  type MaxSize = Int



  def run(p: Prop,
  maxSize: Int = 100,
  testCases: Int = 100,
  rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
    case Falsified(msg, n) =>
    println(s"! Falsified after $n passed tests:\n $msg")
    case Passed =>
    println(s"+ OK, passed $testCases tests.")
    }


  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Passed else Falsified("()", 0)
  }


  //val s1 =  
  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()), choose(1,4).map(Executors.newFixedThreadPool))(_ => p)


  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p,p2)(_ == _)

  def forAllPar[A](g: Gen[A], S: Gen[ExecutorService])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case (s,a) => f(a)(s).get }

  

  // def forAll[A](gen: SGen[A])(f: A => Boolean): Prop = forAll(gen(_))(f)
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) => randomStream(gen)(rng).zipWithUF(Stream.from(0)).take(n).map{
      case (a,i) =>
      try {
        if(f(a)) Passed else Falsified(a.toString, i)
      } catch{
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

  //create random int, and level of nesting, fork and combine with + i guess. Will create nested computations "Get" is called
  // Example
  // map2(fork( fork(unit(X)).map2(Fork())  ), fork(  ))(_ + _)

  // if(random == 4 and depth < n) fork(pint2())

  def lazyInt : Gen[Par[Int]] = choose(500,1000).map(v => Par.lazyUnit({ Thread.sleep(v); v}))
  //seems this is not parallel, the problem is a generator waits on previous generators state, need to generate the bvalues first and then add these values to parallel structure.
  def pint2(depth: Int = 0, max: Int = 8): Gen[Par[Int]] = choose(0,4)
    .flatMap(i =>  
      if(depth >= max || i == 4 ) lazyInt
      else pint2(depth + 1, max).map2(pint2(depth + 1, max)){ (p1, p2)  => Par.map2(p1, p2)(_ + _)})
  
  // choose(0, 987654).map2(choose(1,4)){
  //   (i,d) => ??? 
  // }


  def randomStream[A](g: Gen[A])(rng: RNG) : Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String = 
    s"test case: $s \n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"



    //maybe check head for min value, and last element for max. or even every element such that e_n < e_n + 1
  def sortedInts : Prop = forAll(listOf(int))(l => l.sorted == l.sorted.sorted)



  // EXAMPLES
  val p2 = checkPar{
    equal(
      Par.unit(2),
      Par.map(Par.unit(1))(_ + 1)
    )
  }

/*
  import java.util.concurrent.Executors
  import fpinscala.testing2.Gen._
  import fpinscala.testing2.Prop._
  import fpinscala.testing2._
  import fpinscala.parallelism.Par

  val ES = Executors.newFixedThreadPool(13)
  val r1 = Prop.pint2()
  val p1 = Prop.forAllPar(r1, Gen.unit(ES))(a => Par.map(a)(b => b == 23))
  Prop.run(p1, 1,1)


  */
}

case class Gen[A](sample: State[RNG,A]) {
  def flatMap[B](f: A => Gen[B]) : Gen[B] = Gen(this.sample.flatMap(a => f(a).sample))
  def listOfN(size: Gen[Int]) : Gen[List[A]] = size.flatMap(n => Gen.listOfN(n, this))
  def unsized: SGen[A] = SGen(_ => this)

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def **[B](g: Gen[B]): Gen[(A,B)] =
    (this map2 g)((_,_))
}





object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State(rng => (a, rng)))
  //if start is negative we're screwed.
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))

  def boolean : Gen[Boolean] = Gen(State(RNG.int).map(n => if(n > 0) true else false))

  def int: Gen[Int] = Gen(State(RNG.int))
  //create x generators/ List.fill(n)(g) and lift with sequence
  def listOfN[A](n: Int, g: Gen[A]) : Gen[List[A]] = Gen(State.sequence(List.fill(n)(g).map(g => g.sample)))

  def listOf[A](g: Gen[A]) : SGen[List[A]] =  SGen(n => listOfN(n,g))
  def listOf1[A](g: Gen[A]) : SGen[List[A]] =  SGen(n => listOfN(n max 1,g))
  
  def someGen : Gen[Int] = Gen(State(rng => (2, rng)))

  def union[A](g1: Gen[A], g2: Gen[A]) = boolean.flatMap(b => if(b) g1 else g2)

  

  
  // use boolean,  

  //def option[A](g: Gen[A]): Gen[Option[A]] = boolean.flatMap(b => if(b.sample) Some(g.sample) else None)
}





case class SGen[A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)

  def map[B](f: A => B): SGen[B] =
    SGen { g(_) map f }

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val g2: Int => Gen[B] = n => {
      g(n) flatMap { f(_).g(n) }
    }
    SGen(g2)
  }
}

