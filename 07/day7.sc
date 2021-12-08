def cost1(distance: Int) = distance

def cost2(distance: Int) = (1 + distance) * distance / 2

@main def main(filePath: String, part: Int) = {
  println(s"Day 7 (The Treachery of Whales) part $part using file '$filePath'")
  val textLines = scala.io.Source.fromFile(filePath, "UTF-8").getLines.toList
  val crabPositions = textLines.head.split(",").map(_.toInt)
  val alignedPositionCandidates = crabPositions.min to crabPositions.max

  val minCost = alignedPositionCandidates
    .map(alignedPos =>
      crabPositions
        .map(crabPos => Math.abs(crabPos - alignedPos))
        .map(distance => (if part == 2 then cost2 else cost1)(distance))
        .sum
    )
    .min(Ordering.Int)

  println(minCost)
}
