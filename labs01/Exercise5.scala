import leon.lang._
import leon.annotation._

object SearchList {
  sealed abstract class List[T] {
    def size: BigInt = {
      this match {
        case Nil() => 0
        case Cons(h, t) => 1 + t.size
      }
    } ensuring { _ >= 0 }

    def content: Set[T] = {
      this match {
        case Nil() => Set()
        case Cons(h, t) => Set(h) ++ t.content
      }
    }

    def firstPosOf(v: T): BigInt = {
      this match {
        case Nil() => -1
        case Cons(h, t) if h == v => 0
        case Cons(h, t) =>
          val p = t.firstPosOf(v)
          if (p >= 0) {
            p + 1
          } else {
            p
          }
      }
    } ensuring { res => // DONE
      if (res < 0) !this.contains(v)
      else this.contains(v)
    }

    def take(n: BigInt): List[T] = {
      require(n >= 0)
      this match {
        case Nil() => Nil()
        case Cons(h, t) if n == 0 => Nil()
        case Cons(h, t) => Cons(h, t.take(n - 1))
      }
    } ensuring {
      res => res.size <= n
    }

    def contains(v: T): Boolean = {
      this match {
        case Nil() => false
        case Cons(h, _) if h == v => true
        case Cons(_, t) => t.contains(v)
      }
    } ensuring {
      res => res == (content contains v)
    }
  }

  case class Cons[T](h: T, t: List[T]) extends List[T]
  case class Nil[T]() extends List[T]

  @induct
  def wtf[T](l: List[T], v: T): Boolean = {
    !((l.contains(v)) && (l.take(l.firstPosOf(v)).contains(v))) // What is this function checking? Translate to english. Can you remove the l.contains(v) part? Why?
    /* We can take the following steps:
     * 1. !((l.contains(v)) && (l.take(l.firstPosOf(v)).contains(v))) ( or !(P and Q) )
     * 2. !(l.contains(v)) || !(l.take(l.firstPosOf(v)).contains(v)) ( or (not(P) or not(Q))
     * 3. We can the say that either:
     *  1. l does not contains v or
     *  2. we find the first position of v in l and return false if the sub-list from the beginning of l to the position of v contains v, true o/w.
     * However, they are not equivalent. Consider the following :
     *          l	 := 	Cons[T](T#1, Nil[T]())
     *          v	 := 	T#1
     *  Then !(l.contains(v)) would return false, but the firstPosOf(v) in l is 0, which is passed to take. Take therefore returns Nil(), and
     *  !(Nil.contains(v)) returns true. We can check that using the following def, which returns
     */
  }.holds

  @induct
  def counterExample: Boolean = {
    val l = Cons(1, Nil())
    val v = 1
    !(l.contains(v)) != !(l.take(l.firstPosOf(v)).contains(v))
  }.holds
  
}

