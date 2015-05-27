import leon._
import leon.lang._
import leon.annotation._
import leon.collection._
import leon.collection.ListOps._

/*
 * Implementation of Binomial Heap based on "Purely Functionnal Data Structure, Okasaki, P68+"
 * @author Maëlle Colussi
 * @author Mathieu Demarne
 */

object BinomialHeapBISpec {

  def testInsert {
    val b1 = BinHeap.empty.insert(4)
    assert(b1.size == 1)
  }

  def testMin {
    val b1 = BinHeap.empty.insert(4).insert(2).insert(0).insert(3)
    assert(b1.size == 4)
    assert(b1.findMin == 0)

    val b2 = b1.deleteMin
    assert(b2.size == 3)
    assert(b2.findMin == 2)

    val (e1, b3) = (b2.findMin, b2.deleteMin)
    assert(e1 == 2)
    assert(b3.size == 2)
    assert(b3.findMin == 3)

    val b4 = b3.deleteMin.deleteMin
    assert(b4.size == 0)
  }
  
  /*
  def test { //BinHeap of size 8
	  BinHeap(Cons(
	  Tree(3, 0, 
		Cons(Tree(2, ..., 
			Cons(Tree(1, ..., 
				Cons(Tree(0, ..., Nil()), Nil())), 
			Cons(Tree(0, ..., Nil()), Nil()))), 
		Cons(Tree(1, ..., 
			Cons(Tree(0, ..., Nil()), Nil())), 
		Cons(Tree(0, ..., Nil()), Nil()))))
	  , Nil()))
  }
  */
  /*
  def test {
	  val b3 = BinHeap(Cons(
	  Tree(3, 0, 
		Cons(Tree(2, 1, 
			Cons(Tree(1, 5, 
				Cons(Tree(0, 7, Nil()), Nil())), 
			Cons(Tree(0, 6, Nil()), Nil()))), 
		Cons(Tree(1, 2, 
			Cons(Tree(0, 4, Nil()), Nil())), 
		Cons(Tree(0, 3, Nil()), Nil()))))
	  , Nil()))
	assert(b3.findMin == 0)
	assert(b3.deleteMin.findMin == 1)
    assert(b3.deleteMin.deleteMin.findMin == 2)
    //b3.deleteMin.deleteMin.deleteMin //could reproduce the error

	val b2 = BinHeap( //should be b3.deleteMin
		Cons(Tree(0, 3, Nil()), 
		Cons(Tree(1, 2,
			Cons(Tree(0, 4, Nil()), Nil())), 
		Cons(Tree(2, 1, 
			Cons(Tree(1, 5, 
				Cons(Tree(0, 7, Nil()), Nil())), 
			Cons(Tree(0, 6, Nil()), Nil()))), Nil()))))
	assert(b2.findMin == 1)
	assert(b2.deleteMin.findMin == 2)
    //b2.deleteMin.deleteMin //could reproduce the error

    val b1 = BinHeap(
		Cons(Tree(1, 3, 
			Cons(Tree(0, 6, Nil()), Nil())),
		Cons(Tree(2, 2,
			Cons(Tree(1, 5, 
				Cons(Tree(0, 7, Nil()), Nil())), 
			Cons(Tree(0, 4, Nil()), Nil()))), 
		Nil())))
    assert(b1.findMin == 2)
    //b1.deleteMin //could reproduce the error

    //unrolling deleteMin
    val (n, ts) = Ops.getMin(b1.trees)
    //BinHeap(Ops.merge(n.children.reverse, ts)) //could reproduce the error

    val childList = Cons(Tree(1, 5, Cons(Tree(0, 7, Nil()), Nil())), 
    Cons(Tree(0, 4, Nil()), Nil()))
    //Ops.merge(childList.reverse, ts) //could reproduce the error


    val revChildlist = Cons(Tree(0, 4, Nil()), Cons(Tree(1, 5, Cons(Tree(0, 7, Nil()), Nil())), Nil()))
    //Ops.merge(revChildlist, ts) //could reproduce the error
    val restTree = Cons(Tree(1, 3, 
			Cons(Tree(0, 6, Nil()), Nil())), Nil())

    //unrolling Ops.merge, case t1.rank < t2.rank
    //Tree(0, 4, Nil()) :: Ops.merge(Cons(Tree(1, 5, Cons(Tree(0, 7, Nil()), Nil())), Nil()), restTree) //does not reproduce the error !!


    //gives that as result in the console, when at the end, without error:
    //List(Tree(0, 4, List[Tree]()), Tree(2, 3, List(Tree(1, 5, List(Tree(0, 7, List[Tree]()))), Tree(0, 6, List[Tree]()))))
    val res1 = Cons(Tree(0, 4, Nil()), Cons(Tree(2, 3, Cons(Tree(1, 5, Cons(Tree(0, 7, Nil()), Nil())), Cons(Tree(0, 6, Nil()), Nil()))), Nil()))
    //assert(Ops.hasProperShape(res1))
    assert(Ops.hasMinHeapProp(res1))
    assert(Ops.hasIncrRanks(res1)) //this one makes problem, in fact: ?!?


    //result of the recursive call, which passes the postcondition
    val partResRight = Cons(Tree(2, 3, Cons(Tree(1, 5, Cons(Tree(0, 7, Nil()), Nil())), Cons(Tree(0, 6, Nil()), Nil()))), Nil())
    assert(Ops.hasProperShape(partResRight))

  }*/

  def testMerge {
    val b1 = BinHeap.empty.insert(4).insert(2).insert(0).insert(3)
    val b2 = BinHeap.empty.insert(1).insert(6).insert(5).insert(7)

    val b3 = b1 merge b2
    assert(b3.size == 8)

    assert(b3.findMin == 0)
    assert(b3.deleteMin.findMin == 1)
    assert(b3.deleteMin.deleteMin.findMin == 2)
    assert(b3.deleteMin.deleteMin.deleteMin.findMin == 3)
    assert(b3.deleteMin.deleteMin.deleteMin.deleteMin.findMin == 4)
    assert(b3.deleteMin.deleteMin.deleteMin.deleteMin.deleteMin.findMin == 5)
    assert(b3.deleteMin.deleteMin.deleteMin.deleteMin.deleteMin.deleteMin.findMin == 6)
    assert(b3.deleteMin.deleteMin.deleteMin.deleteMin.deleteMin.deleteMin.deleteMin.findMin == 7)

    /*val b4 = b3.deleteMin.deleteMin
    val b5 = BinHeap.empty.insert(8).insert(9).insert(0).insert(10)
    val b6 = BinHeap.empty.insert(1).insert(12).insert(11).insert(13)
    val b7 = (b6 merge b5) merge b4
    val b8 = (b4 merge b6) merge b5

    assert(b8.findMin == b7.findMin)
    assert(b8.deleteMin.findMin == b7.deleteMin.findMin)
    assert(b8.deleteMin.deleteMin.findMin == b7.deleteMin.deleteMin.findMin)*/
  }

}
