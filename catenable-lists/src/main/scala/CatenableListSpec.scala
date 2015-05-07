import leon._
import leon.lang._
import leon.annotation._
import leon.collection._
import leon.collection.ListOps._

/*
 * Implementation of Catenable List based on "Purely Functionnal Data Structure, Okasaki, P93+" - test file
 * @author Maëlle Colussi
 * @author Mathieu Demarne
 */

 // TODO: check if it would be nice to have intermediate parameterized tests.
object CatenableListSpec {

  def testSnocCons {
      val l0 = CEmpty[BigInt]()
      val l1 = l0.snoc(4).snoc(5)
      val l2 = l0.snoc(5).cons(4)
      assert(l1 == l2)
  }

  def testMergeAndHead {
  	  val l1 = CEmpty[BigInt]().snoc(1).snoc(2).snoc(3)
  	  val l2 = CEmpty[BigInt]().snoc(4).snoc(5).snoc(6)
  	  val lt1 = l1 ++ l2
      assert(lt1.content == l1.content ++ l2.content)
  	  
  	  val l3 = CEmpty[BigInt]().snoc(7).snoc(8).snoc(9)
  	  val lt2 = lt1 ++ l3
      assert(lt2.content == l1.content ++ l2.content ++ l3.content)
  	  
  	  val lt3 = lt2.cons(0)
      assert(lt3.head == 0)
      assert(lt3.size == 10)
  }

  // TODO: check why this is unkown, it does not take any parameter and should therefore be
  // executed.
  def testHeadAndTail {
  	val l1 = CEmpty[BigInt]().snoc(3).snoc(4).cons(2).snoc(5).snoc(6).cons(1)
  	val l2 = l1 ++ CEmpty[BigInt]().snoc(8).cons(7)
  	val l3 = CEmpty[BigInt]().snoc(0).cons(-1) ++ l2

  	assert(l3.size == 10)
  	assert(l3.head == -1)
    val ll3 = l3.tail
  	//assert(l3.tail.head == 0)
  	//assert(l3.tail.tail.head == 1)
  	//assert(l3.tail.tail.tail.tail.tail.tail.head == 5)
  }

  def testTail {
  	val l1 = CEmpty[BigInt]().cons(3)
    assert(l1.tail.isEmpty)
  }
}
