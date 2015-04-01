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

	def content: Set[T] = this match {
		case CEmpty() => Set()
		case CCons(h, t) => Set(h) ++ t.toList.flatMap(_.content.toList).content
	}
	
	def size: BigInt = this match {
		case CEmpty() => 0
		case CCons(h, t) => 1 + t.size
	}

	def head: T = {
		require(this.isDefined)
		this match {
			case CCons(h, t) => h
		}
	} ensuring(res => this.content.contains(res)) //TODO : more ?

	def tail: CatenableList[T] = {
		require(this.isDefined)
		this match {
			case CCons(h, t) if t.isEmpty => CEmpty()
			case CCons(h, t) => linkAll(t)
		}
	} ensuring(res => res.content.forall{x => this.content.contains(x)} && res.size == this.size - 1)// TODO: more ? structure perhaps
	
	
	def linkAll(q: Queue[CatenableList[T]]): CatenableList[T] = { 
		require(q.isDefined)
		q.tail match {
			case QEmpty() => q.head
			case qTail @ QCons(f, r) => q.head.link(linkAll(qTail))
		}
	} ensuring(res => res.content == q.content && res.size == q.size) //TODO : more ? on structure

	def cons(x: T): CatenableList[T] = {
		CCons(x, QEmpty[CatenableList[T]]) ++ this
	} ensuring(res => res.content == Set(x) ++ this.content && res.size == this.size + 1) // TODO: more ?

	def snoc(x: T): CatenableList[T] = {
		this ++ CCons(x, QEmpty[CatenableList[T]]) 
	} ensuring(res => res.content == Set(x) ++ this.content && res.size == this.size + 1)// TODO: more ?

	def ++ (that: CatenableList[T]): CatenableList[T] = {
		(this, that) match {
			case (CEmpty(), _ ) => that
			case (_, CEmpty()) => this
			case _ => this.link(that)
		}	
	} ensuring(res => res.content == this.content ++ that.content && res.size == this.size + that.size) // TODO: more ?
	
	def link(that: CatenableList[T]): CatenableList[T] = {
		require(this.isDefined && that.isDefined)
		this match {
			case CCons(h, t) => CCons(h, t.snoc(that)) //TODO : p96 : "tree suspension"
		}
	}  ensuring(res => res.content == this.content ++ that.content && res.size == this.size + that.size) // TODO: more ? on structure
	
}

case class CCons[T](h: T, t: Queue[CatenableList[T]]) extends CatenableList[T]
case class CEmpty[T]() extends CatenableList[T]
