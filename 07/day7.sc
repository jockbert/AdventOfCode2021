def cost1(distance: Int) = distance

def cost2(distance: Int) = (1 + distance) * distance / 2

@main def main(filePath: String, part: Int) = {
  val isPart2 = part.equals(2)
  println(s"AOC day 7 part $part, with file '$filePath'")
  val textLines = scala.io.Source.fromFile(filePath, "UTF-8").getLines.toList

  val crabPositions = textLines.head.split(",").map(_.toInt)

  val alignedPositionCandidates = crabPositions.min to crabPositions.max

  val minCost = alignedPositionCandidates
    .map(al =>
      crabPositions
        .map(cr => {
          val distance = Math.abs(cr - al)
          (if isPart2 then cost2 else cost1)(distance)
        })
        .sum
    )
    .min(Ordering.Int)

  println(minCost)
}
