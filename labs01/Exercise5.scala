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
      if (res >= 0) this.contains(v)
      else !this.contains(v)
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
    !((l.contains(v)) && (l.take(l.firstPosOf(v)).contains(v)))
    // What is this function checking? Translate to english. Can you remove the l.contains(v) part? Why?

    /* It verifies that firstPosOf returns, if v is in l:
     *  - a position which is before the first v in l or;
     *  - the first position of v in l.
     *
     * Removing l.contains(v) changes the meaning of the check as it breaks the requirement of take(). Below is an
     * example:
     *      l -> Cons[T](T#0, Nil[T]())
     *      v -> T#1
     * l.firstPosOf(v) return -1, but take has as requirement that n is greater or equal to 0.
     *
     * Having l.contains(v) in the formula enforces that we check firstPosOf only if v is in l. For instance, the
     * example above would check.
     *
     */
  }.holds

  @induct
  def counterExample: Boolean = {
    val l = Cons(1, Nil())
    val v = 1
    !(l.contains(v)) != !(l.take(l.firstPosOf(v)).contains(v))
  }.holds

}

