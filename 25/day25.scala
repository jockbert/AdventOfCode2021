import scala.annotation.tailrec

case class Point(x: Int, y: Int) {
  def south(size: Point) = Point(x, if (y >= size.y - 1) 0 else y + 1)
  def east(size: Point) = Point(if (x >= size.x - 1) 0 else x + 1, y)
  def allWithin = (0 until y).flatMap(b => (0 until x).map(a => Point(a, b)))
}

case class State(easters: Set[Point], southers: Set[Point], size: Point) {
  def advance = {
    val newEasters = easters.map(p => {
      val q = p.east(size)
      if (easters.contains(q) || southers.contains(q)) p else q
    })
    val newSouthers = southers.map(p => {
      val q = p.south(size)
      if (newEasters.contains(q) || southers.contains(q)) p else q
    })
    State(newEasters, newSouthers, size)
  }
}

@tailrec def searchLockUpLoop(s: State, loopCount: Int = 1): Int =
  val s2 = s.advance
  if (s2 == s) loopCount else searchLockUpLoop(s2, loopCount + 1)

@main def main(filePath: String) = {
  println(s"Day 25 (Sea Cucumber) using file '$filePath'")
  val textLines = scala.io.Source.fromFile(filePath, "UTF-8").getLines.toList
  val size = Point(textLines.head.size, textLines.size)
  val easters = size.allWithin.filter(p => textLines(p.y)(p.x) == '>').toSet
  val southers = size.allWithin.filter(p => textLines(p.y)(p.x) == 'v').toSet
  val start = State(easters, southers, size)

  println("Part 1 - lock up at loop number " + searchLockUpLoop(start))
}
