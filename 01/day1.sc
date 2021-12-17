def countIncreases(depths: List[Int]): Int =
  depths.sliding(2).count(ds => ds(1) > ds(0))

@main def main(filePath: String) =
  println(s"Day 1 (Sonar Sweep) using file '${filePath}'")
  val lines = scala.io.Source.fromFile(filePath, "UTF-8").getLines.toList
  val depths = lines.map(_.toInt)
  val depthsSlidingWindow = depths.sliding(3).map(_.sum).toList
  println("Part 1: " + countIncreases(depths))
  println("Part 2: " + countIncreases(depthsSlidingWindow))
