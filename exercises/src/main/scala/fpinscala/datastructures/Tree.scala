package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
    
    //count leaves and branches
    def size[T](t: Tree[T]): Int = t match  { 
      case Leaf(_) => 1
      case Branch(left, right) => size(left) + size(right) 
    }
    def maximum[T](t: Tree[T], bigger : (T,T) => Boolean): T = {
      def maxInner(ti: Tree[T]) : T = ti match {
        case Leaf(v) => v
        case Branch(left, right) => if(bigger(maxInner(left), maxInner(right))) maxInner(left) else maxInner(right)
      }
      maxInner(t)
    }
    def depth[T](t: Tree[T]): Int = t match {
      case Leaf(_) => 1
      case Branch(left, right) => { val ld = 1 + depth(left); val rd = 1 + depth(right); if(ld > rd) ld else rd }
    }
    def map[T, Y](t: Tree[T], f : T => Y) : Tree[Y] = t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(left, right) => Branch(map(left, f), map(right, f))
    }
    
    val r1 = 2
    


}