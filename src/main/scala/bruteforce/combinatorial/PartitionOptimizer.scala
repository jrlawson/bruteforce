package bruteforce.combinatorial

class PartitionOptimizer(size: Int, objectiveFunction: Subset=>Option[Double]) {
  def optimize: Option[(Subset,Double)] = {
    var bestCase: Option[(Subset,Double)] = None

    def evaluate(subset: Subset): Unit = {
      objectiveFunction(subset) match {
        case None => ;
        case Some(objectiveValue) =>
          bestCase match {
            case None =>
              bestCase = Some((subset, objectiveValue))
            case Some(best) =>
              if (objectiveValue < best._2) {
                bestCase = Some(subset, objectiveValue)
              }
          }
      }
    }
    var subset: Option[Subset] = BinarySubset(size)
    while (subset.isDefined) {
      evaluate(subset.get)
      subset = subset.get.next
    }
    bestCase
  }
}


object PartitionObjectiveFunction {
  def objective[T](localCost: (T, T) => Double)(seq: Seq[T])(p: Subset): Option[Double] =
    if (p.n == seq.length) {
      var sum = 0.0
      //for (i <- 1 until p.n) sum += localCost(seq(p(i - 1)), seq(p(i)))
      Some(sum)
    } else {
      None
    }
}