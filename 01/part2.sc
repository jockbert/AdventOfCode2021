import scala.annotation.tailrec

@tailrec
def countIncrements(last: Int, depths: Iterator[Int], increments: Int): Int = {
  if (!depths.hasNext) then increments
  else
    val next = depths.next()
    countIncrements(next, depths, increments + (if last < next then 1 else 0))
}

@main def day1(filePath: String) =
  println(s"AOC day 1 part 2, with file '${filePath}'")
  val lines = scala.io.Source.fromFile(filePath, "UTF-8").source.getLines
  val depths = lines.map(_.toInt)
  val d3 = depths.sliding(3).map(_.sum)
  println(countIncrements(d3.next, d3, 0))
