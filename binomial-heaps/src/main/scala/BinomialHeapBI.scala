import leon._
import leon.lang._
import leon.annotation._
import leon.collection._

/*
 * Implementation of Binomial Heap based on "Purely Functionnal Data Structure, Okasaki, P68+"
 * @author Maëlle Colussi
 * @author Mathieu Demarne
 */

//to use T : add a mapping from T to BigInt

sealed abstract class BinomialHeapBI {
	
	/* Lower-level API */

	def isEmpty: Boolean = this match {
		case BHList(Nil()) => true
		case _ => false
	}
	def isDefined: Boolean = !this.isEmpty

	def insert(x: BigInt): BinomialHeapBI = {
		require(this.minHeapPropBH && this.uniqueRanks)
		this.insTree(TreeNode(0, x, BHList(Nil())))
	} ensuring (res => res.size == this.size + 1 && res.content == Set(x) ++ this.content && res.minHeapPropBH && res.uniqueRanks)

	protected def insTree(t1: TreeBI): BinomialHeapBI = {//ts: this
		require(this.minHeapPropBH && this.uniqueRanks)
		this match {
			case BHList(Nil()) => BHList(Cons(t1, Nil()))
			case a @ BHList(Cons(t2, rest)) => {
				if (t1.rank < t2.rank) BHList(Cons(t1, a.f))
				else BHList(rest).insTree(t1.link(t2))
			}
		}
	} ensuring (res => res.size == this.size + t1.size && res.content == this.content ++ t1.content && res.minHeapPropBH && res.uniqueRanks)

	def merge(that: BinomialHeapBI): BinomialHeapBI = {
		require(this.minHeapPropBH && that.minHeapPropBH && this.uniqueRanks)
		(this, that) match {
			case (BHList(t), BHList(Nil())) => BHList(t)
			case (BHList(Nil()), BHList(t)) => BHList(t)
			case (BHList(Cons(t1, ts1)), BHList(Cons(t2, ts2))) => {
				if (t1.rank < t2.rank)  BHList(Cons(t1, 
					(BHList(ts1).merge(that)) match { case BHList(f) => f }))
				else if (t2.rank < t1.rank) BHList(Cons(t2, 
					(this.merge(BHList(ts2))) match {case BHList(f) => f }))
				else BHList(ts1).merge(BHList(ts2)).insTree(t1.link(t2))
			}
		}
	} ensuring (res => res.size == this.size + that.size && res.content == this.content ++ that.content && res.minHeapPropBH && res.uniqueRanks)

	def findMin: BigInt = {
		require(this.isDefined && this.minHeapPropBH && this.uniqueRanks)
		this match {
			case BHList(Cons(t, Nil())) => t.root
			case BHList(Cons(t, ts)) => {
				val x = t.root
				val y = BHList(ts).findMin
				if (x <= y) x else y
			}
		}
	} ensuring(res => this.content.contains(res) && this.toList.forall(x => x >= res))

	def deleteMin: BinomialHeapBI = {
		require(this.isDefined && this.minHeapPropBH && this.uniqueRanks)
		this.findAndDeleteMin._2
	} ensuring (res => res.size == this.size - 1 && res.minHeapPropBH && res.uniqueRanks)

	def findAndDeleteMin: (BigInt, BinomialHeapBI) = {
		require(this.isDefined && this.minHeapPropBH && this.uniqueRanks)
		this.getMin match {
			case (TreeNode(_, x, ts1), ts2) => 
				(x, ts1.reverse.merge( BHList(ts2)))
		}
	} ensuring (res => res._2.size == this.size - 1 && res._2.minHeapPropBH && res._2.uniqueRanks)

	protected def getMin: (TreeBI, List[TreeBI]) = {
		require(this.isDefined && this.minHeapPropBH && this.uniqueRanks)
		this match {
			case BHList(Cons(t, Nil())) => (t, Nil())
			case BHList(Cons(t, ts)) => {
				BHList(ts).getMin match {
					case (tp, tsp) => {
						if (t.root <= tp.root) (t, ts)
						else (tp, Cons(t, tsp))
					}
				}
			}
		}
	} ensuring(res => BHList(res._2).toList.forall(x => x >= res._1.root) && (res._1 :: res._2).forall(x => x.minHeapPropTree && x.uniqueRankTree))

	protected def reverse: BinomialHeapBI = {
		require(this.minHeapPropBH && this.uniqueRanks)
		this match {
			case BHList(f) => BHList(f.reverse)
		}
	} ensuring (res => res.size == this.size && res.content == this.content && res.minHeapPropBH  && res.uniqueRanks)

	def size: BigInt = {
		this match {
			case BHList(Nil()) => 0
			case BHList(f) => BinomialHeapBI.sumInList(f, 0)
		}
	} ensuring (_ >= 0)

	/* Structure transformation */

	def toList: List[BigInt] = {
		this match {
			case BHList(Nil()) => Nil()
			case a @ BHList(f) => {
				val (min, restBI) = a.findAndDeleteMin
				Cons(min, restBI.toList)
			}
		}
	} ensuring (_.size == this.size)

	def content: Set[BigInt] = {
		this match {
			case BHList(Nil()) => Set()
			case BHList(Cons(t, ts)) => t.content ++ BHList(ts).content
		}
	}
	
	def forall(func: TreeBI => Boolean): Boolean = this match {
		case BHList(Nil()) => true
		case BHList(Cons(t, ts)) => func(t) && BHList(ts).forall(func(_))
	}
	
	def minHeapPropBH: Boolean = this.forall(_.minHeapPropTree)
	
	//There can only be either one or zero binomial trees for each order, 
	//including zero order.
	//And ranks are >= 0
	def uniqueRanks: Boolean = this match {
		case BHList(Nil()) => true
		case BHList(f) => {
			val ranks = f.map(_.rank)
			ranks.forall(x => !ranks.--(List(x)).contains(x)) && ranks.forall(_ >= 0)
		}
	}

}

/* Companion object */
object BinomialHeapBI {

	/* Lower-leve API */

	def empty = BHList(Nil[TreeBI]())
	
	/* Helpers */

	private def sumInList[T](lst: List[TreeBI], acc: BigInt): BigInt = {
		require(acc >= 0)
		lst match {
			case Nil() => acc
			case Cons(h, t) => sumInList(t, acc + h.size)
		}
	} ensuring(_ >= 0)
	
}

case class BHList(f : List[TreeBI]) extends BinomialHeapBI
