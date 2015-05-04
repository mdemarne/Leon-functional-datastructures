import leon._
import leon.lang._
import leon.annotation._
import leon.collection._

/*
 * Implementation of Binomial Heap based on "Purely Functionnal Data Structure, Okasaki, P68+"
 * @author Maëlle Colussi
 * @author Mathieu Demarne
 */

sealed abstract class BinomialHeapBI {

	def isEmpty: Boolean = this == BHEmpty()
	def isDefined: Boolean = !this.isEmpty
	def isFormallyOk: Boolean = this match {
		case BHList(Cons(t2, Nil())) => false
		case _ => true
	}
	
	def insert(x: BigInt): BinomialHeapBI = {
		require(this.isFormallyOk)
		this.insTree(TreeNode(0, x, BHEmpty()))
	}
	
	def insTree(t1: TreeBI): BinomialHeapBI = {//ts: this
		require(this.isFormallyOk)
		this match {
			case a @ BHList(Cons(t2, rest)) => {
				if (t1.rank < t2.rank) BHList(Cons(t1, a.f))
				else BHList(rest).insTree(t1.link(t2))
			}
			case BHEmpty() => BHList(Cons(t1, Nil()))
		}
	}
	def merge(that: BinomialHeapBI): BinomialHeapBI = {
		require(this.isFormallyOk && that.isFormallyOk)
		(this, that) match {
			case (BHList(t), BHList(Nil())) => BHList(t)
			case (BHList(Nil()), BHList(t)) => BHList(t)
			case (BHList(Cons(t1, ts1)), BHList(Cons(t2, ts2))) => {
				if (t1.rank < t2.rank)  BHList(Cons(t1, (BHList(ts1).merge(that)) match {
					case BHList(f) => f }))
				else if (t2.rank < t1.rank) BHList(Cons(t2, (this.merge(BHList(ts2))) match {
					case BHList(f) => f }))
				else BHList(ts1).merge(BHList(ts2)).insTree(t1.link(t2))
			}
		}
	}
	def findMin(): BigInt = {
		require(this.isDefined && this.isFormallyOk)
		this match {
			case BHList(Cons(t, Nil())) => t.root()
			case BHList(Cons(t, ts)) => {
				val x = t.root()
				val y = BHList(ts).findMin()
				if (x <= y) x else y
			}
		}
	}
	def deleteMin(): BinomialHeapBI = {
		require(this.isDefined && this.isFormallyOk)
		this.getMin() match {
			case (TreeNode(_, x, ts1), ts2) => 
				ts1.reverse().merge( BHList(ts2))
		}
	}
	def getMin(): (TreeBI, List[TreeBI]) = {
		require(this.isDefined && this.isFormallyOk)
		this match {
			case BHList(Cons(t, Nil())) => (t, Nil())
			case BHList(Cons(t, ts)) => {
				BHList(ts).getMin() match {
					case (tp, tsp) => {
						if ( t.root() <= tp.root()) (t, ts)
						else (tp, Cons(t, tsp))
					}
				}
			}
		}
	}
	def reverse(): BinomialHeapBI = {
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

case class BHList(f : List[TreeBI]) extends BinomialHeapBI
case class BHEmpty() extends BinomialHeapBI
