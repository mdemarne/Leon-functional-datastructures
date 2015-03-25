import leon._
import leon.lang._
import leon.annotation._
import leon.collection._
/* 
 * Implementation of Catenable List based on "Purely Functionnal Data Structure, Okasaki, P93+" 
 * @author Maëlle Colussi
 * @author Mathieu Demarne
 */

sealed abstract class CatenableList[T] {

	def isEmpty: Boolean = this == Empty[T]()

	def isDefined: Boolean = !this.isEmpty

	def head: T = {
		require(this.isDefined)
		this match {
			case Cons(h, t) => h
		}
	} // TODO: postcondition

	def tail: Queue[T] = {
		require(this.isDefined)
		this match {
			case Cons(h, t) if t.isEmpty => Empty
			case Cons(h, t) => linkAll(t)
		}
	} // TODO: postcondition
	
	
	def linkAll(q: Queue[T]): Queue[T] = { 
		require(q.isDefined)
		q.tail match {
			case Empty => q.head
			case qTail @ Cons(f, r) => link(q.head, linkAll(qTail))
			}
	}

	def cons(x: T): Queue[T] = Cons(x, Empty) ++ this // TODO: postcondition
	def snoc(x: T): Queue[T] = this ++ Cons(x, Empty) // TODO: postcondition

	def ++ (that: CatenableList[T]) = {
		(this, that) match {
			case (Empty, _ ) => that
			case (_, Empty) => this
			case _ => this.link(that)
		}	
	}  // TODO: postcondition
	
	def link(that: CatenableList[T]){
		require(this.isDefined && that.isDefined)
		this match {
			case Cons(h, t) => Cons(h, t.snoc(that)) //TODO : p96 : "tree suspension"
		}
	}  // TODO: postcondition
	
}

case class Cons[T](h: T, t: Queue[CatenableList[T]]) extends CatenableList[T]
case class Empty[T]() extends Queue[CatenableList[T]]
