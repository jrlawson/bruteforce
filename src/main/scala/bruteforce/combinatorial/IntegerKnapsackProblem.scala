package bruteforce.combinatorial

object IntegerKnapsackObjectiveFunction {
  def objective(problem: IntegerKnapsackProblem)(subset: Subset): Option[Double] = {
    val items = subset.members.size
    var lootValue: Int = 0
    var lootSize: Int = 0
    val iterator = subset.iterator
    while (iterator.hasNext) {
      problem.item(iterator.next) match {
        case Some(item) => lootValue = lootValue - item._1
          lootSize = lootSize + item._2
        case _ => ;
      }
    }
    if (lootSize <= problem.capacity) Some(lootValue.doubleValue)
    else None
  }
}
class IntegerKnapsackProblem(loot: Array[(Int,Int)], cap: Int) {
  def n: Int = loot.length
  def capacity: Int = cap
  def item(i: Int): Option[(Int, Int)] = {
    if (i<0) None
    else if (i>=n) None
    else Some(loot(i))
  }

  def itemValue(i: Int): Option[Int] = item(i) match {
    case None => None
    case Some(item) => Some(item._1)
  }

  def itemSize(i: Int): Option[Int] = item(i) match {
    case None => None
    case Some(item) => Some(item._2)
  }
}

object IntegerKnapsackProblemExample {
  def printSolution(solution: (Subset, Double)): Unit = {
    println("Knapsack value = " + solution._2)
    println("   Items = " + solution._1.k)
    val iterator = solution._1.iterator
    while (iterator.hasNext) {
      val item = iterator.next
      println("        Item " + item + "       value = " + loot(item)._1 + "       size = " + loot(item)._2)
    }
  }

  val loot: Array[(Int,Int)] = Array((1,5),(2,4),(3,3),(4,2),(5,1))
  val knapsackCapacity = 7
  def main(args: Array[String]): Unit = {
    val problem = new IntegerKnapsackProblem(loot, knapsackCapacity)
    val objectiveFunction = IntegerKnapsackObjectiveFunction.objective(problem)(_)
    val optimizer = new PartitionOptimizer(problem.n, objectiveFunction)
    optimizer.optimize() match {
      case Some(solution) =>
        printSolution(solution)
      case None => println("No solution found")
    }
  }
}
