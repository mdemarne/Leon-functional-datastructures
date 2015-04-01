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

	/* lower-level API */

	def isEmpty: Boolean = this == QEmpty[T]()

	def isDefined: Boolean = !this.isEmpty

	def size: BigInt = {
		require(this.hasFrontOrEmpty)
		this match {
			case QEmpty() => 0
			case QCons(f, r) => f.size + r.size
		}
	} ensuring (res => res == this.toList.size && res >= 0)

	def head: T = {
		require(this.isDefined && this.hasFrontOrEmpty)
		this match {
			case QCons(f, r) => f.head
		}
	}

	def tail: Queue[T] = {
		require(this.isDefined && this.hasFrontOrEmpty)
		this match {
			case QCons(Cons(e, Nil()), r) if r.isEmpty => QEmpty()
			case QCons(Cons(e, Nil()), r) => QCons(r.reverse, Nil())
			case QCons(Cons(e, es), r) => QCons(es, r)
		}
	} ensuring (res => res.hasFrontOrEmpty && res.size + 1 == this.size)

	def snoc(x: T): Queue[T] = {
		require(this.hasFrontOrEmpty)
		this match {
			case QEmpty() =>  QCons(x :: Nil(), Nil())
			case QCons(f, r) => QCons(f, x :: r)
		}
	} ensuring (res => res.isDefined && res.hasFrontOrEmpty && res.size - 1 == this.size)

	def toList: List[T] = (this match {
		case QEmpty() => Nil()
		case QCons(f, r) => f ++ r
	}) ensuring (res => res.size == this.size && res.size >= 0)


	def content: Set[T] = (this match {
		case QEmpty() => Set()
		case QCons(f, r) => f.content ++ r.content
	}) ensuring (res => res == this.toList.content)

	/* Higher-order API */

	def map[R](func: T => R): Queue[R] = (this match {
		case QEmpty() => QEmpty()
		case QCons(f, r) => QCons(f.map(func(_)), r.map(func(_)))
	}) ensuring (_.size == this.size)

	/* Invariants */

	def hasFrontOrEmpty = this match {
		case QEmpty() => true
		case QCons(f, r) => !f.isEmpty
	}
}

case class QCons[T](f : List[T], r: List[T]) extends Queue[T]
case class QEmpty[T]() extends Queue[T]
