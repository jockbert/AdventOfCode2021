type PairCounts = Map[String, BigInt]

def polymerize(
    original: PairCounts,
    rules: Map[String, List[String]]
): PairCounts =
  // Explode to non-unique list of element-pair-count-tuple that are grouped
  // together to a map again.
  original.toList
    .flatMap(prCnt => rules(prCnt._1).map(newPr => (newPr, prCnt._2)))
    .groupMapReduce(_._1)(_._2)(_ + _)

@main def main(filePath: String) = {
  println(s"Day 14 (Extended Polymerization) using file '$filePath'")
  val textLines = scala.io.Source.fromFile(filePath, "UTF-8").getLines.toList

  val polymerTemplate = textLines.head
    .sliding(2)
    .toList
    .groupMapReduce(_.toString)(_ => BigInt(1))(_ + _)

  val rules = textLines
    .drop(2)
    .map(_.split(" -> "))
    .map(a => (a(0), "" + a(0)(0) + a(1) :: a(1) + a(0)(1) :: Nil))
    .toMap

  def elementQuantities(steps: Int): BigInt = {
    val finalPolymer =
      (1 to steps).foldLeft(polymerTemplate)((p, _) => polymerize(p, rules))

    val lastElem = textLines.head.last

    // Since we only pick/count the first element in element pair, we will miss
    // to count last element occurrence in the polymer. That is compensated for
    // in last map below.
    val elementCounts = finalPolymer.toList
      .map((pair, cnt) => (pair(0), cnt))
      .groupMapReduce(_._1)(_._2)(_ + _)
      .map((elem, cnt) => (elem, cnt + (if elem == lastElem then 1 else 0)))

    //println("element count: " + elementCounts)
    val countsOnly = elementCounts.map(_._2).toList
    countsOnly.max - countsOnly.min
  }

  println("Step 1 (10 iterations): " + elementQuantities(10))
  println("Step 2 (40 iterations): " + elementQuantities(40))
}
