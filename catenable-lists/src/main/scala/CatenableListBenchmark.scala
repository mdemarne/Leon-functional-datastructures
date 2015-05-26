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

// NOTE: Compile with SCALAC, not LEON
object CatenableListBenchmark extends App {

  if (args.isEmpty) sys.error("The test to run should be passed to the benchmark")

  args.head match {
    case "consNormalList" =>
    case "snocNormalList" =>
    case "concatNormalList" =>
    case "headNormalList" =>
    case "consCatList" =>
    case "snocCatList" =>
    case "concatCatList" =>
    case "headCatList" =>
  }
  // TODO: use scalac for this one; might need to do changes in Leon for that
  // TODO: take an input parameter to run benchmarks (choose which one to run)
  // TODO: Have a test for: (list vs CatList)
  // 1. Cons 
  // 2. ++
  // 3. Cons and head
  // TODO: do a shell script that will call this with various argument flavours and
  // plot results using shell command line.
}


