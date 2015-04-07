import leon._
import leon.lang._
import leon.annotation._
import leon.collection._

/* 
 * Implementation of Catenable List based on "Purely Functionnal Data Structure, Okasaki, P93+" 
 * @author Maëlle Colussi
 * @author Mathieu Demarne
 */

 // TODO: 1) verify and finish all structures
 // TODO: 2) add better checks, of course!

sealed abstract class CatenableList[T] {

	/* Lower-level API */

	// OK
	def isEmpty: Boolean = this == CEmpty[T]()

	// OK
	def isDefined: Boolean = !this.isEmpty

	// TODO: NOT OK
	/*def content: Set[T] = this match {
		case CEmpty() => Set()
		// TODO: change once we have (if we do) define flatMap on queues
		case CCons(h, t) => Set(h) ++ t.toList.flatMap(_.toList).content
	}*/

	// TODO: NOT OK
	/*def toList: List[T] = this match {
		case CEmpty() => Nil()
		case CCons(h, t) => h :: t.toList.flatMap(_.toList)
	}*/
	
	// TODO: NOT OK
	/*def size: BigInt = this match {
		case CEmpty() => 0
		case CCons(h, t) => 1 + t.size
	}*/

	// OK
	def cons(x: T): CatenableList[T] = {
		require(this.hasProperTailOrEmpty)
		CCons(x, QEmpty[CatenableList[T]]()) ++ this
	} //ensuring(res => res.content == Set(x) ++ this.content && res.size == this.size + 1) // TODO: more ?

	// OK
	def snoc(x: T): CatenableList[T] = {
		require(this.hasProperTailOrEmpty)
		this ++ CCons(x, QEmpty[CatenableList[T]]()) 
	} //ensuring(res => res.content == Set(x) ++ this.content && res.size == this.size + 1)// TODO: more ?

	// OK
	def ++ (that: CatenableList[T]): CatenableList[T] = {
		require(this.hasProperTailOrEmpty && that.hasProperTailOrEmpty)
		(this, that) match {
			case (CEmpty(), _) => that
			case (_, CEmpty()) => this
			case _ => this.link(that)
		}	
	} //ensuring(res => res.content == this.content ++ that.content && res.size == this.size + that.size) // TODO: more ?

	// OK
	def head: T = {
		require(this.isDefined && this.hasProperTailOrEmpty)
		this match {
			case CCons(h, t) => h
		}
	} //ensuring(res => this.content.contains(res)) //TODO: more ?


	// OK
	def tail: CatenableList[T] = {
		require(this.isDefined && this.hasProperTailOrEmpty)
		this match {
			case CCons(h, t) if t.isEmpty => CEmpty()
			case CCons(h, t) => CatenableList.linkAll(t)
		}
	} //ensuring(res => res.content.forall{x => this.content.contains(x)} && res.size == this.size - 1) // TODO: more ? structure perhaps

	/* Helpers */

	// TODO: OK
	private def link(that: CatenableList[T]): CatenableList[T] = {
		require(this.isDefined && this.hasProperTailOrEmpty && that.isDefined && that.hasProperTailOrEmpty)
		this match {
			case CCons(h, t) => CCons(h, t.snoc(that)) //TODO : p96 : "tree suspension"
		}
	} //ensuring(res => res.content == this.content ++ that.content && res.size == this.size + that.size) // TODO: more ? on structure

	/* Invariants */

	def hasProperTailOrEmpty = {
		this match {
			case CEmpty() => true
			case CCons(h, t) => t.hasFrontOrEmpty
		}
	} // TODO

}

/* Companion object */
object CatenableList {

	/* Helpers */

	// OK
	def linkAll[T](q: Queue[CatenableList[T]]): CatenableList[T] = { 
		require(q.isDefined && q.hasFrontOrEmpty)
		q.tail match {
			case QEmpty() => q.head
			case qTail => q.head.link(linkAll(qTail))
		}
	} //ensuring(res => res.content == q.content && res.size == q.size) //TODO : more ? on structure
	
}

case class CCons[T](h: T, t: Queue[CatenableList[T]]) extends CatenableList[T]
case class CEmpty[T]() extends CatenableList[T]
