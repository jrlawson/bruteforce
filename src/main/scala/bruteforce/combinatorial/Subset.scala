package bruteforce.combinatorial

trait Subset {                // Of the integers from 0 to n-1
  def n: Int                  // Size of the set
  def k: Int                  // Size of the subset
  def members: Set[Int]       // Elements of the subset
  def from: Set[Int]          // Elements of the set
  def next: Option[Subset]    // Next subset
  def iterator: Iterator[Int] // Iterator over the subset
}

