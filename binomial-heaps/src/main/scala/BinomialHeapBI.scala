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

	def isEmpty: Boolean = this match {
		case BHList(Nil()) => true
		case _ => false
	}
	def isDefined: Boolean = !this.isEmpty

	def insert(x: BigInt): BinomialHeapBI = {
		this.insTree(TreeNode(0, x, BHList(Nil())))
	} ensuring (res => res.size == this.size + 1) //TODO : more ?

	def insTree(t1: TreeBI): BinomialHeapBI = {//ts: this
		this match {
			case BHList(Nil()) => BHList(Cons(t1, Nil()))
			case a @ BHList(Cons(t2, rest)) => {
				if (t1.rank < t2.rank) BHList(Cons(t1, a.f))
				else BHList(rest).insTree(t1.link(t2))
			}
		}
	} ensuring (res => res.size == this.size + t1.size) //TODO : more ?

	def merge(that: BinomialHeapBI): BinomialHeapBI = {
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
	} ensuring (res => res.size == this.size + that.size) //TODO : more ?

	def findMin(): BigInt = {
		require(this.isDefined)
		this match {
			case BHList(Cons(t, Nil())) => t.root()
			case BHList(Cons(t, ts)) => {
				val x = t.root()
				val y = BHList(ts).findMin()
				if (x <= y) x else y
			}
		}
	} //ensuring () //TODO

	def deleteMin(): BinomialHeapBI = {
		require(this.isDefined)
		this.getAndDeleteMin()._2
	} ensuring (res => res.size == this.size - 1) //TODO : more ?

	def getAndDeleteMin(): (BigInt, BinomialHeapBI) = {
		require(this.isDefined)
		this.getMin() match {
			case (TreeNode(_, x, ts1), ts2) => 
				(x, ts1.reverse().merge( BHList(ts2)))
		}
	} ensuring (res => res._2.size == this.size - 1) //TODO : more ?

	def getMin(): (TreeBI, List[TreeBI]) = {
		require(this.isDefined)
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
	} //ensuring () //TODO

	def reverse(): BinomialHeapBI = {
		this match {
			case BHList(f) => BHList(f.reverse)
		}
	} ensuring (res => res.size == this.size)

	def size: BigInt = {
		this match {
			case BHList(Nil()) => 0
			case BHList(f) => sumInList(f, 0)
		}
	} ensuring (res => /*res == this.toList.size &&*/ res >= 0)

	private def sumInList[T](lst: List[TreeBI], acc: BigInt): BigInt = {
		require(acc >= 0)
		lst match {
			case Nil() => acc
			case Cons(h, t) => sumInList(t, acc + h.size)
		}
	} ensuring(_ >= 0)

	def toList: List[BigInt] = {
		this match {
			case BHList(Nil()) => Nil()
			case a @ BHList(f) => {
				val (min, restBI) = a.getAndDeleteMin()
				Cons(min, restBI.toList)
			}
		}
	} ensuring (res => /*this.content == res.content &&*/ res.size == this.size && res.size >= 0)

	def content: Set[BigInt] = {
		this match {
			case BHList(Nil()) => Set()
			case BHList(Cons(t, ts)) => t.content ++ BHList(ts).content
		}
	} ensuring (res => res == this.toList.content)

}

case class BHList(f : List[TreeBI]) extends BinomialHeapBI
