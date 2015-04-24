import leon._
import leon.lang._
import leon.annotation._
import leon.collection._
import leon.collection.ListOps._

/*
 * Implementation of Queue based on "Purely Functionnal Data Structure, Okasaki, P15+"
 * @author Maëlle Colussi
 * @author Mathieu Demarne
 */

 // TODO: pass --xlang as option to Leon for inline expansion
object QueueSpec {

  def test1 {
      //TODO: do better testing!
      val Q1 = QEmpty[Int]()
      val Q2 = Q1.snoc(4)
      if (Q2.size != 1) error[Unit]("Wrong size!")
  }
  
  def testOrder1 {
	  val Q1 = QEmpty[BigInt]()
	  val Q2 = Q1.snoc(1).snoc(2).snoc(3)
	  val Q3 = Q1.snoc(4).snoc(5).snoc(6)
	  val Q4 = Q2 ++ Q3
	  val listQ4: List[BigInt] = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Nil()))))))
	  if(Q4.toList.content != listQ4.content || !isSorted(Q4.toList)) 
		error[Unit]("Wrong order!")
  }
  
  def testOrder2 {
	  val Q1 = QCons[BigInt](Cons(1, Cons(2, Cons(3, Nil()))), Cons(6, Cons(5, Cons(4, Nil()))))
	  val Q2 = QCons[BigInt](Cons(7, Cons(8, Cons(9, Nil()))), Cons(12, Cons(11, Cons(10, Nil()))))
	  val Q3 = Q1 ++ Q2
	  val listQ1: List[BigInt] = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Nil()))))))
	  val listQ2: List[BigInt] = Cons(7, Cons(8, Cons(9, Cons(10, Cons(11, Cons(12, Nil()))))))
	  if (Q3.toList.content != listQ1.content ++ listQ2.content || !isSorted(Q3.toList)) 
		error[Unit]("Wrong order 2!")
  }
  
  def testTail {
	  val Q1 = QCons[BigInt](Cons(1, Cons(2, Cons(3, Nil()))), Cons(5, Cons(4, Nil())))
	  val listQ1: List[BigInt] = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil())))))
	  val Q2 = Q1.tail
	  val listQ2: List[BigInt] = Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))
	  if (Q2.toList.content != listQ2.content || !isSorted(Q2.toList)) 
		error[Unit]("Wrong order 3.1!")
	  val Q3 = Q2.tail
	  val listQ3: List[BigInt] = Cons(1, Cons(2, Cons(3, Nil())))
	  if (Q3.toList.content != listQ3.content || !isSorted(Q3.toList)) 
		error[Unit]("Wrong order 3.2!")
	  val Q4 = Q3.tail
	  val listQ4: List[BigInt] = Cons(1, Cons(2, Nil()))
	  if (Q4.toList.content != listQ4.content || !isSorted(Q4.toList)) 
		error[Unit]("Wrong order 3.3!")
	  //after snoc
	  val Q5 = Q4.snoc(3)
	  if (Q5.toList.content != listQ3.content || !isSorted(Q5.toList)) 
		error[Unit]("Wrong order 3.4!")
  }

}
