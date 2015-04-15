import leon._
import leon.lang._
import leon.annotation._
import leon.collection._

/*
 * Implementation of Catenable List based on "Purely Functionnal Data Structure, Okasaki, P93+"
 * @author Maëlle Colussi
 * @author Mathieu Demarne
 */

 // DONE: 1) verify and finish all structures
 // TODO: 2) add better checks and add external func (content, toList, etc.)!
 
 // TODO: comparisons of contents in ensuring makes java exception
 // TODO: linkAll ensuring

sealed abstract class CatenableList[T] {

	/* Lower-level API */

	def isEmpty: Boolean = this == CEmpty[T]()

	def isDefined: Boolean = !this.isEmpty

	def size: BigInt = {
		require(this.hasProperShape)
		this match {
			case CEmpty() => 0
			case CCons(h, t) => 1 + CatenableList.sumTail(t)
		}
	} ensuring (_ >= 0) // TODO: more ?

	def cons(x: T): CatenableList[T] = {
		require(this.hasProperShape)
		CCons(x, QEmpty[CatenableList[T]]()) ++ this
	} ensuring(res => /*res.content == Set(x) ++ this.content &&*/ res.size == this.size + 1) // TODO: more ?


	def snoc(x: T): CatenableList[T] = {
		require(this.hasProperShape)
		this ++ CCons(x, QEmpty[CatenableList[T]]())
	} ensuring(res => /*res.content == Set(x) ++ this.content &&*/ res.size == this.size + 1)// TODO: more ?

	def ++(that: CatenableList[T]): CatenableList[T] = {
		require(this.hasProperShape && that.hasProperShape)
		(this, that) match {
			case (CEmpty(), _) => that
			case (_, CEmpty()) => this
			case _ => this.link(that)
		}
	} ensuring(res => /*res.content == this.content ++ that.content &&*/ res.size == this.size + that.size) // TODO: more ?

	def head: T = {
		require(this.isDefined && this.hasProperShape)
		this match {
			case CCons(h, t) => h
		}
	} //ensuring(res => this.content.contains(res)) //TODO: more ?


	def tail: CatenableList[T] = {
		require(this.isDefined && this.hasProperShape)
		this match {
			case CCons(h, t) if t.isEmpty => CEmpty()
			case CCons(h, t) => CatenableList.linkAll(t)
		}
	} ensuring(res => /*res.toList.forall{x => this.content.contains(x)} &&*/ res.size == this.size - 1) // TODO: more ? structure perhaps

	def content: Set[T] = this match {
		case CEmpty() => Set()
		case CCons(h, t) => Set(h) ++ (t.toList.flatMap(_.toList)).content // TODO: check why this is not properly typed according to the compiler.
	}


	def toList: List[T] = this match {
		case CEmpty() => Nil()
		case CCons(h, t) => Cons(h, t.toList.flatMap(_.toList)) // TODO: check why this is not properly typed according to the compiler.
	}

	/* Helpers */

	private def link(that: CatenableList[T]): CatenableList[T] = {
		require(this.isDefined && this.hasProperShape && that.isDefined && that.hasProperShape)
		this match {
			case CCons(h, t) => CCons(h, t.snoc(that)) //TODO : p96 : "tree suspension"
		}
	} ensuring(res => /*res.content == this.content ++ that.content &&*/ res.size == this.size + that.size) // TODO: more ? on structure

	/* Invariants */

	def hasProperShape = this match {
		case CEmpty() => true
		/* The queue must have proper shape according to queue specs, and we cannot have a queue of empty lists */
		case CCons(h, t) => CatenableList.queueHasProperShapeIn(t)
	}

}

/* Companion object */
object CatenableList {

	/* Helpers */

	def linkAll[T](q: Queue[CatenableList[T]]): CatenableList[T] = {
		require(q.isDefined && queueHasProperShapeIn(q))
		q.tail match {
			case QEmpty() => q.head
			case qTail => q.head.link(linkAll(qTail))
		}
	} //ensuring(res => res.content == q.content && res.size == q.size) //TODO : more ? 
		//problem q.content gives CatenableList, not T

	def sumTail[T](q: Queue[CatenableList[T]]): BigInt = {
		require(queueHasProperShapeIn(q))
		q match {
			case QEmpty() => 0
			case QCons(f, r) => sumInList(f, 0) + sumInList(r, 0) 
		}
	} ensuring(_ >= 0)

	//there were problems with foldleft in leon, so we use this function
	private def sumInList[T](lst: List[CatenableList[T]], acc: BigInt): BigInt = {
		require(lst.forall(_.hasProperShape) && acc >= 0)
		lst match {
			case Nil() => acc
			case Cons(h, t) => sumInList(t, acc + h.size)
		}
	} ensuring(_ >= 0) // TODO

	/* Invariants */

	def queueHasProperShapeIn[T](q: Queue[CatenableList[T]]): Boolean = {
		q.hasProperShape && q.forall(x => x.isDefined && x.hasProperShape)
	}
}

case class CCons[T](h: T, t: Queue[CatenableList[T]]) extends CatenableList[T]
case class CEmpty[T]() extends CatenableList[T]
