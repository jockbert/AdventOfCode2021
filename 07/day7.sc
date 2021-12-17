def cost1(distance: Int) = distance
def cost2(distance: Int) = (1 + distance) * distance / 2

@main def main(filePath: String) = {
  println(s"Day 7 (The Treachery of Whales) using file '$filePath'")
  val textLines = scala.io.Source.fromFile(filePath, "UTF-8").getLines.toList
  val crabPositions = textLines.head.split(",").map(_.toInt)
  val alignedPositionCandidates = crabPositions.min to crabPositions.max

  def calcMinCost(fn: Int => Int) = alignedPositionCandidates
    .map(alignedPos =>
      crabPositions.map(crabPos => Math.abs(crabPos - alignedPos)).map(fn).sum
    )
    .min(Ordering.Int)

  println("part 1: " + calcMinCost(cost1))
  println("part 2: " + calcMinCost(cost2))
}
