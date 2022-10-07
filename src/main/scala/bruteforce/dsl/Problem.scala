package bruteforce.dsl

class Input[T](name: String) {

}
abstract class Problem {
  def name = "Problem"
  def inputs: List[Input[T:>Any]]
}
