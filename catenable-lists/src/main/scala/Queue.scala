import leon._
import leon.lang._
import leon.annotation._
import leon.collection._

/* 
 * Implementation of Queue based on "Purely Functionnal Data Structure, Okasaki, P15+" 
 * @author Maëlle Colussi
 * @author Mathieu Demarne
 */

sealed abstract class Queue[T] {

	def isEmpty: Boolean = this == Empty[T]()

	def isDefined:Boolean = !this.isEmpty

	def head: T = {
		require(this.isDefined && this.hasFrontOrEmpty)
		this match {
			case Cons(f, r) => f.head
		}
	} // TODO: postcondition

	def tail: Queue[T] = {
		require(this.isDefined && this.hasFrontOrEmpty)
		this match {
			case Cons(Nil(), r) => Cons(r.reverse, Nil())
			case Cons(f, r) => Cons(f.tail, r)
		}
	} // TODO: postcondition

	def snoc(x: T): Queue[T] = this match {
		case Empty() =>  Cons(x :: Nil(), Nil())
		case Cons(f, r) => Cons(f, x :: r)
	}  // TODO: postcondition


	def hasFrontOrEmpty = this match {
		case Empty() => true
		case Cons(f, r) => !f.isEmpty
	}
	
}

case class Cons[T](f : List[T], r: List[T]) extends Queue[T]
case class Empty[T]() extends Queue[T]
