import leon._
import leon.lang._
import leon.annotation._
import leon.collection._

/*
 * Implementation of Tree based on "Purely Functionnal Data Structure, Okasaki, P71+"
 * @author Maëlle Colussi
 * @author Mathieu Demarne
 */

sealed abstract class TreeBI {
	def link(that: TreeBI) : TreeBI = {//t1: this, t2:that
		require(this.correctFormTree && that.correctFormTree && this.rank == that.rank)
		(this, that) match {
			case (TreeNode(r, x1, c1), TreeNode(r1, x2, c2)) if r == r1 =>
				if (x1 <= x2) TreeNode(r + 1, x1, BHList(Cons(that, 
					c1 match {case BHList(f) => f})))
				else TreeNode(r + 1, x2, BHList(Cons(this, 
					c2 match {case BHList(f) => f})))
		}
	} ensuring (res => res.size == this.size + that.size && res.correctFormTree)

	def rank: BigInt = this match {
		case TreeNode(r, x, c) => r
	}

	def root: BigInt = this match {
		case TreeNode(r, x, c) => x
	}

	def size: BigInt = {
		this match {
			case TreeNode(r, x, c) => 1 + c.size
		}
	} ensuring (_ >= 0)

	def toList: List[BigInt] = {
		require(this.correctFormTree)
		this match {
			case TreeNode(r, x, c) => Cons(r, c.toList)
		}
	} ensuring (res => res.size == this.size)

	def content: Set[BigInt] = {
		this match {
			case TreeNode(r, x, c) => Set(x) ++ c.content
		}
	}
	
	/* Invariants */
	
	def correctFormTree: Boolean = this.minHeapPropTree && this.uniqueRankTree

	//Each binomial tree in a heap obeys the minimum-heap property: 
	//the key of a node is greater than or equal to the key of its parent.
	def minHeapPropTree: Boolean = this match {
		case TreeNode(r, x, BHList(Nil())) => true
		case TreeNode(r, x, BHList(f)) => f.forall(_.root >= x) && BHList(f).minHeapPropBH
	}
	
	def uniqueRankTree: Boolean = this match {
		case TreeNode(r, x, BHList(Nil())) => r >= 0
		case TreeNode(r, x, BHList(f)) => r >= 0 && BHList(f).uniqueRanks
	}
}

case class TreeNode(r: BigInt, x: BigInt, c: BinomialHeapBI) extends TreeBI

