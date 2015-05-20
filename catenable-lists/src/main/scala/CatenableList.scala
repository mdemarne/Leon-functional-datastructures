import leon._
import leon.lang._
import leon.annotation._
import leon.collection._

/*
 * Implementation of Catenable List based on "Purely Functionnal Data Structure, Okasaki, P93+"
 * Properties: efficient concatenation functions in O(1) (Snoc, Cons, ++)
 * @author Maëlle Colussi
 * @author Mathieu Demarne
 */

 // DONE: 1) verify and finish all structures
 // DONE: 2) add better checks and add external func (content, toList, etc.)!
 // 						SOLVED 1. Some problems (weird errors from Leon side) using content.
 //							DONE 2. Adding some tests based on forall instead of content.
 // DONE: add more Spec tests.

 // DONE: comparisons of contents of sets in ensuring makes java exception
 //		=> problem came from the use of flatMaps. Using recursive functions instead.
 // DONE: solving issue with flatMap and foldLeft with Etienne (problem Leon-side on types)
 // TODO: flatMaps and foldLefts are not suited for proof verification due to closures, which
 // have no precondition.

sealed abstract class CatenableList[T] {

	/* Lower-level API */

	def isEmpty: Boolean = this == CEmpty[T]()

	def isDefined: Boolean = !this.isEmpty

	def size: BigInt = {
		require(this.hasProperShape)
		val res: BigInt = this match {
			case CEmpty() => 0
			case CCons(h, t) => 
				// TODO: It would be nice to use foldLeft, but due to closures, we can't do it directly.
				// 1 + t.foldLeft(BigInt(0))((x, y) => x + y.size)
				1 + CatenableList.sumTail(t)
		}
		res
	} ensuring (_ >= 0)

	def cons(x: T): CatenableList[T] = {
		require(this.hasProperShape)
		CCons(x, QEmpty[CatenableList[T]]()) ++ this
	} ensuring(res => res.content == this.content ++ Set(x) && res.head == x && res.size == this.size + 1)

	def snoc(x: T): CatenableList[T] = {
		require(this.hasProperShape)
		this ++ CCons(x, QEmpty[CatenableList[T]]())
	} ensuring(res => res.content == this.content ++ Set(x) && res.size == this.size + 1)

	def ++(that: CatenableList[T]): CatenableList[T] = {
		require(this.hasProperShape && that.hasProperShape)
		(this, that) match {
			case (CEmpty(), _) => that
			case (_, CEmpty()) => this
			case _ => this.link(that)
		}
	} ensuring(res => res.content == this.content ++ that.content && res.size == this.size + that.size)

	def head: T = {
		require(this.isDefined && this.hasProperShape)
		this match {
			case CCons(h, t) => h
		}
	} ensuring(res => this.contains(res))


	def tail: CatenableList[T] = {
		require(this.isDefined && this.hasProperShape)
		val res: CatenableList[T] = this match {
			case CCons(h, t) if t.isEmpty => CEmpty()
			case CCons(h, t) => CatenableList.linkAll(t)
		}
		res
	} ensuring(res => (res.forall(this.contains(_)) || res == CEmpty[T]()) && res.size == this.size - 1)

	/* Structure transformation */

	def content: Set[T] = {
		require(this.hasProperShape)
		val res: Set[T] = this match {
			case CEmpty() => Set()
			case CCons(h, t) =>
				// TODO: It would be nice to use flatMap, but due to closures, we can't do it directly.
				// Set(h) ++ (t.toList.flatMap(_.toList)).content
				Set(h) ++  CatenableList.queueOfCatToContent(t)
		}
		res
	}

	def toList: List[T] = {
		require(this.hasProperShape)
		val res: List[T] = this match {
			case CEmpty() => Nil()
			case CCons(h, t) => 
				// TODO: It would be nice to use flatMap, but due to closures, we can't do it directly.
				// h :: (t.toList.flatMap(_.toList))
				h ::  CatenableList.queueOfCatToList(t)
		}
		res
	} ensuring(res => res.content == this.content)

	/* high-level API */

	def forall(func: T => Boolean): Boolean = this match {
		case CEmpty() => true
		case CCons(h, t) => func(h) && t.forall(_.forall(func))
	}

	def contains(x: T): Boolean = this match {
		case CEmpty() => false
		case CCons(h, t) if h == x => true
		case CCons(h, t) => t.exists(_.contains(x))
	}

	/* Helpers */

	private def link(that: CatenableList[T]): CatenableList[T] = {
		require(this.isDefined && this.hasProperShape && that.isDefined && that.hasProperShape)
		this match {
			case CCons(h, t) => CCons(h, t.snoc(that)) // TODO : p96 : "tree suspension"
		}
	} ensuring(res => res.content == this.content ++ that.content && res.size == this.size + that.size)

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
	} ensuring(res => q.forall(_.forall(res.contains(_))))

	def sumTail[T](q: Queue[CatenableList[T]]): BigInt = {
		require(queueHasProperShapeIn(q))
		val res: BigInt = q match {
			case QEmpty() => 0
			case QCons(f, r) => sumInList(f, 0) + sumInList(r, 0)
		}
		res
	} ensuring(_ >= 0)

	private def sumInList[T](lst: List[CatenableList[T]], acc: BigInt): BigInt = {
		require(lst.forall(_.hasProperShape) && acc >= 0)
		lst match {
			case Nil() => acc
			case Cons(h, t) => sumInList(t, acc + h.size)
		}
	} ensuring(_ >= 0)
	
	def queueOfCatToContent[T](q: Queue[CatenableList[T]]): Set[T] =  {
		require(queueHasProperShapeIn(q))
		q match {
			case QEmpty() => Set()
			case QCons(l, r) => listOfCatToContent(l) ++ listOfCatToContent(r)
		}
	}

	private def listOfCatToContent[T](l: List[CatenableList[T]]): Set[T] = {
		require(l.forall(_.hasProperShape))
		l match {
			case Nil() => Set()
			case Cons(h, t) => h.content ++ listOfCatToContent(t)
		}
	}

	def queueOfCatToList[T](q: Queue[CatenableList[T]]): List[T] = {
		require(queueHasProperShapeIn(q))
		q match {
			case QEmpty() => Nil()
			case QCons(l, r) => listOfCatToList(l) ++ listOfCatToList(r)
		}
	}

	private def listOfCatToList[T](l: List[CatenableList[T]]): List[T] = {
		require(l.forall(_.hasProperShape))
		l match {
			case Nil() => Nil()
			case Cons(h, t) => h.toList ++ listOfCatToList(t)
		}
	}

	/* Invariants */

	def queueHasProperShapeIn[T](q: Queue[CatenableList[T]]): Boolean = {
		q.hasProperShape && q.forall(x => x.isDefined && x.hasProperShape)
	}
}

case class CCons[T](h: T, t: Queue[CatenableList[T]]) extends CatenableList[T]
case class CEmpty[T]() extends CatenableList[T]
