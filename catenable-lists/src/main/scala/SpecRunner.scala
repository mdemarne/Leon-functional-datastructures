import leon._
import leon.lang._
import leon.annotation._
import leon.collection._
import leon.collection.ListOps._

/*
 * Testing implementation calling the various specs - for compilation with scalac.
 * NOTE TO COMPILE: pass all the leon library in command line after modifying the calls
 * as mentionned in https://stackoverflow.com/questions/29990106/how-do-i-build-with-scalac-using-leons-library.
 * @author Maëlle Colussi
 * @author Mathieu Demarne
 */

object SpecRunner extends App {

	println("Checking QueueSpec...")
	QueueSpec.testSnoc
	QueueSpec.testOrder1
	QueueSpec.testOrder2
	QueueSpec.testTail

	println("Checking CatenableListSpec...")
	CatenableListSpec.testSnocCons
	CatenableListSpec.testMergeAndHead
	CatenableListSpec.testHeadAndTail
	CatenableListSpec.testTail
}
