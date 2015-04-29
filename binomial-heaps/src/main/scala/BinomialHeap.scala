import leon._
import leon.lang._
import leon.annotation._
import leon.collection._

/*
 * Implementation of Binomial Heap based on "Purely Functionnal Data Structure, Okasaki, P68+"
 * @author Maëlle Colussi
 * @author Mathieu Demarne
 */




sealed abstract class BinomialHeap[T] {

	def isEmpty: Boolean = this == BHEmpty[T]()
	def isDefined: Boolean = !this.isEmpty
	//def size: BigInt = {???} ensuring (res => res == this.toList.size && res >= 0)

	def insert(x: T): BinomialHeap[T] = {???}
	def merge(that: BinomialHeap[T]): BinomialHeap[T] = {???}
	def findMin(): T = {???}
	def deleteMin(): BinomialHeap[T] = {???} 
	
	
	//def toList: List[T] = {???} ensuring (res => this.content == res.content && res.size == this.size && res.size >= 0)
	//def content: Set[T] = {???} ensuring (res => res == this.toList.content /*&& res.size == this.toList.size*/)



}

case class BHList[T](f : List[Tree[T]]) extends BinomialHeap[T]
case class BHEmpty[T]() extends BinomialHeap[T]
