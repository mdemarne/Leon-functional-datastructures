import leon._
import leon.lang._
import leon.annotation._
import leon.collection._

/*
 * Implementation of Queue based on "Purely Functionnal Data Structure, Okasaki, P15+"
 * @author Maëlle Colussi
 * @author Mathieu Demarne
 */


/* TODO: Not sure that defining flatten is the best solution
object QueueOps {
	def flatten[T](que: Queue[Queue[T]]): Queue[T] = que match {
		require(que.hasProperShape && que.forall(_.hasProperShape))
		case QEmpty() => QEmpty()
		case QCons(f, r) => ???
	}
}*/

sealed abstract class Queue[T] {

	/* Lower-level API */

	def isEmpty: Boolean = this == QEmpty[T]()

	def isDefined: Boolean = !this.isEmpty

	def size: BigInt = {
		require(this.hasProperShape)
		this match {
			case QEmpty() => 0
			case QCons(f, r) => f.size + r.size
		}
	} ensuring (res => res == this.toList.size && res >= 0)

	def head: T = {
		require(this.isDefined && this.hasProperShape)
		this match {
			case QCons(f, r) => f.head
		}
	} ensuring (res => this.content.contains(res))

	def tail: Queue[T] = {
		require(this.isDefined && this.hasProperShape)
		this match {
			case QCons(Cons(e, Nil()), r) if r.isEmpty => QEmpty()
			case QCons(Cons(e, Nil()), r) => QCons(r.reverse, Nil())
			case QCons(Cons(e, es), r) => QCons(es, r)
		}
	} ensuring (res => res.hasProperShape && res.size + 1 == this.size)

	def snoc(x: T): Queue[T] = {
		require(this.hasProperShape)
		this match {
			case QEmpty() =>  QCons(x :: Nil(), Nil())
			case QCons(f, r) => QCons(f, x :: r)
		}
	} ensuring (res => res.isDefined && res.hasProperShape && res.size - 1 == this.size)

	// TODO: check if required
	def ++(that: Queue[T]): Queue[T] = {
		require(this.hasProperShape && that.hasProperShape)
		(this, that) match {
			case (QEmpty(), _) => that
			case (_, QEmpty()) => this
			// TODO: might be worth doing the ++ in a lazy manner only, if possible. ++ takes time compared
			// to other accesses in O(1) (snoc, head, tail, etc). Another possibility would be to implement
			// those queues using QCons(List[T], List[List[T]]) for instance! But ++ might very well not be
			// required.
			case (QCons(f1, r1), QCons(f2, r2)) => QCons(f1, r2 ++ f2.reverse ++ r1)
		}
	} ensuring(res => res.hasProperShape && res.size == this.size + that.size && res.content == this.content ++ that.content)

	/* Structure transformation */

	def toList: List[T] = (this match {
		case QEmpty() => Nil()
		case QCons(f, r) => f ++ r.reverse
	}) ensuring (res => this.content == res.content && res.size == this.size && res.size >= 0)


	def content: Set[T] = (this match {
		case QEmpty() => Set()
		case QCons(f, r) => f.content ++ r.content
	}) ensuring (res => res == this.toList.content)

	/* Higher-order API */

	def map[R](func: T => R): Queue[R] = (this match {
		case QEmpty() => QEmpty()
		case QCons(f, r) => QCons(f.map(func(_)), r.map(func(_)))
	}) ensuring (_.size == this.size)

	def forall(func: T => Boolean): Boolean = this match {
		case QEmpty() => true /* Default, as in Scala standards */
		case QCons(f, r) => f.forall(func(_)) && r.forall(func(_))
	}

	def exists(func: T => Boolean): Boolean = this match {
		case QEmpty() => false
		case QCons(f, r) => f.exists(func(_)) || r.exists(func(_))
	}

	/* Invariants */

	def hasProperShape = this match {
		case QEmpty() => true
		case QCons(f, r) => !f.isEmpty
	}
}

case class QCons[T](f : List[T], r: List[T]) extends Queue[T]
case class QEmpty[T]() extends Queue[T]
