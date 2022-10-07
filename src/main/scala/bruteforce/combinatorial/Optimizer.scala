package bruteforce.combinatorial

class Optimizer[S](size: Int, objectiveFunction: S=>Option[Double]) {
  def optimize: Option[(S,Double)] = {
    var bestCase: Option[(S,Double)] = None

    def evaluate(sample: S): Unit = {
      objectiveFunction(sample) match {
        case None => ;
        case Some(objectiveValue) =>
          bestCase match {
            case None =>
              bestCase = Some((sample, objectiveValue))
            case Some(best) =>
              if (objectiveValue < best._2) {
                bestCase = Some(sample, objectiveValue)
              }
          }
      }
    }
    var sample: Option[S] = LexicalOrderPermutation(size)
    while (sample.isDefined) {
      evaluate(sample.get)
      sample = sample.get.next
    }
    bestCase
  }
}
