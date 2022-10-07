package bruteforce.dsl

class Input[T](name: String) {

}
abstract class Problem {
  name = "Problem"
  inputs: List[Input]
}
