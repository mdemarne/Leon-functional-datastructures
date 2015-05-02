import leon._
import leon.lang._
import leon.annotation._
import leon.collection._

/*
 * Implementation of Binomial Heap based on "Purely Functionnal Data Structure, Okasaki, P68+"
 * @author Maëlle Colussi
 * @author Mathieu Demarne
 */
 
 //TODO
 // T <: Ordered[] problem with Test

sealed abstract class BinomialHeap[T <: Ordered[T]] extends Test[T] {

	def isEmpty: Boolean = this == BHEmpty[T]()
	def isDefined: Boolean = !this.isEmpty
	def isFormallyOk: Boolean = this match {
		case BHList(Cons(t2, Nil())) => false
		case _ => true
	}
	
	def insert(x: T): BinomialHeap[T] = {
		require(this.isFormallyOk)
		this.insTree(TreeNode[T](0, x, BHEmpty()))
	}
	
	def insTree(t1: Tree[T]): BinomialHeap[T] = {//ts: this
		require(this.isFormallyOk)
		this match {
			case a @ BHList(Cons(t2, rest)) => {
				if (t1.rank < t2.rank) BHList(Cons(t1, a.f))
				else BHList(rest).insTree(t1.link(t2))
			}
			case BHEmpty() => BHList[T](Cons(t1, Nil()))
		}
	}
	def merge(that: BinomialHeap[T]): BinomialHeap[T] = {
		require(this.isFormallyOk && that.isFormallyOk)
		(this, that) match {
			case (BHList(t), BHList(Nil())) => BHList(t)
			case (BHList(Nil()), BHList(t)) => BHList(t)
			case (BHList(Cons(t1, ts1)), BHList(Cons(t2, ts2))) => {
				if (t1.rank < t2.rank)  BHList(Cons(t1, (BHList(ts1).merge(that)).f /*match {
					case BHList(f) => f }*/))
				else if (t2.rank < t1.rank) BHList(Cons(t2, (this.merge(BHList(ts2))).f /*match {
					case BHList(f) => f }*/))
				else BHList(ts1).merge(BHList(ts2)).insTree(t1.link(t2))
			}
		}
	}
	def findMin(): T = {
		require(this.isDefined && this.isFormallyOk)
		this match {
			case BHList(Cons(t, Nil())) => t.root()
			case BHList(Cons(t, ts)) => {
				val x = t.root()
				val y = BHList(ts).findMin()
				if (/*TOrdering.lteq(x, y)*/ x <= y) x else y
			}
		}
	}
	def deleteMin(): BinomialHeap[T] = {
		require(this.isDefined && this.isFormallyOk)
		this.getMin() match {
			case (TreeNode(_, x, ts1), ts2) => 
				ts1.reverse().merge( BHList(ts2))
		}
	}
	def getMin(): (Tree[T], List[Tree[T]]) = {
		require(this.isDefined && this.isFormallyOk)
		this match {
			case BHList(Cons(t, Nil())) => (t, Nil())
			case BHList(Cons(t, ts)) => {
				BHList(ts).getMin() match {
					case (tp, tsp) => {
						if (/*TOrdering.lteq(t.root(), tp.root())*/ t.root() <= tp.root()) (t, ts)
						else (tp, Cons(t, tsp))
					}
				}
			}
		}
	}
	def reverse(): BinomialHeap[T] = {
		require(this.isFormallyOk)
		this match {
			case BHEmpty() => this
			case BHList(f) => BHList(f.reverse)
		}
	}
	
	//def size: BigInt = {???} ensuring (res => res == this.toList.size && res >= 0)
	//def toList: List[T] = {???} ensuring (res => this.content == res.content && res.size == this.size && res.size >= 0)
	//def content: Set[T] = {???} ensuring (res => res == this.toList.content /*&& res.size == this.toList.size*/)

}

case class BHList[T <: Ordered[T]](f : List[Tree[T]]) extends BinomialHeap[T] with Test[T]
case class BHEmpty[T <: Ordered[T]]() extends BinomialHeap[T] with Test[T] { val f = Nil[Tree[T]]() }

trait Test[T <: Ordered[T]] {val f: List[Tree[T]]}
