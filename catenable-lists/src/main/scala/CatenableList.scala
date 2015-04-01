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

	def isEmpty: Boolean = this == CEmpty[T]()

	def isDefined: Boolean = !this.isEmpty

	def head: T = {
		require(this.isDefined)
		this match {
			case CCons(h, t) => h
		}
	} // TODO: postcondition

	def tail: CatenableList[T] = {
		require(this.isDefined)
		this match {
			case CCons(h, t) if t.isEmpty => CEmpty()
			case CCons(h, t) => linkAll(t)
		}
	} // TODO: postcondition
	
	
	def linkAll(q: Queue[CatenableList[T]]): CatenableList[T] = { 
		require(q.isDefined)
		q.tail match {
			case QEmpty() => q.head
			case qTail @ QCons(f, r) => q.head.link(linkAll(qTail))
		}
	}

	def cons(x: T): CatenableList[T] = CCons(x, QEmpty[CatenableList[T]]) ++ this // TODO: postcondition
	def snoc(x: T): CatenableList[T] = this ++ CCons(x, QEmpty[CatenableList[T]]) // TODO: postcondition

	def ++ (that: CatenableList[T]): CatenableList[T] = {
		(this, that) match {
			case (CEmpty(), _ ) => that
			case (_, CEmpty()) => this
			case _ => this.link(that)
		}	
	}  // TODO: postcondition
	
	def link(that: CatenableList[T]): CatenableList[T] = {
		require(this.isDefined && that.isDefined)
		this match {
			case CCons(h, t) => CCons(h, t.snoc(that)) //TODO : p96 : "tree suspension"
		}
	}  // TODO: postcondition
	
}

case class CCons[T](h: T, t: Queue[CatenableList[T]]) extends CatenableList[T]
case class CEmpty[T]() extends CatenableList[T]
