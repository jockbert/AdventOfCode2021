def countIncreases(depths: List[Int]): Int = depths
  .sliding(2)
  .map({
    case a :: b :: Nil if (a < b) => 1
    case _                        => 0
  })
  .sum

@main def main(filePath: String, part: Int) =
  println(s"Day 1 (Sonar Sweep) part $part using file '${filePath}'")
  val lines = scala.io.Source.fromFile(filePath, "UTF-8").getLines.toList
  val depths = lines.map(_.toInt)
  val depthsSlidingWindow = depths.sliding(3).map(_.sum).toList
  println(countIncreases(if part == 2 then depthsSlidingWindow else depths))
