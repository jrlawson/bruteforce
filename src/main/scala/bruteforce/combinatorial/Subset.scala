package bruteforce.combinatorial

trait Subset {                // Of the integers from 0 to n-1
  def n: Int                  // Size of the set
  def k: Int                  // Size of the subset
  def members: Set[Int]       // Elements of the subset
  def from: Set[Int]          // Elements of the set
  def next: Option[Subset]    // Next subset
  def iterator: Iterator[Int] // Iterator over the subset
}
class BinarySubset private(v: Long, size: Int) extends Subset {
  def n: Int = size
  def k: Int = members.size
  lazy val from: Set[Int] = (0 until n).toSet[Int]
  lazy val iterator: Iterator[Int] = members.iterator

  lazy val members: Set[Int] = {
    val membersBuffer = new scala.collection.mutable.ArrayBuffer[Int]()
    var mask = 1L
    for (i <- 0 until n) {
      if ((v & mask) != 0) membersBuffer += i
      mask <<= 1
    }
    membersBuffer.toSet[Int]
  }

  def next: Option[Subset] = {
    if (v==(1<<n)-1) {
      None
    } else {
      Some(new BinarySubset(v+1, size))
    }
  }
}

object BinarySubset {
  def apply(size: Int): Option[Subset] = {
    if (size<0) None
    else if (size >63) None
    else Some(new BinarySubset(0L, size))
  }

  def main(args: Array[String]): Unit = {
    var subset = BinarySubset(5)
    while (subset.isDefined) {
      println(subset.get.members)
      subset = subset.get.next
    }
  }
}
