case class Point(x: Int, y: Int) {
  def sub(other: Point) = Point(x - other.x, y - other.y)
  def add(ox: Int, oy: Int) = Point(x + ox, y + oy)
}

case class Line(a: Point, b: Point) {

  def isVerticalOrHorizontal = a.x == b.x || a.y == b.y
  def isDiagonal = a.sub(b) match {
    case Point(n, m) if n == m  => true
    case Point(n, m) if n == -m => true
    case _                      => false
  }

  def allPointsOnLine: List[Point] = {
    def rangeZeroTo(n: Int) = (0 to n by n.sign).toList
    b.sub(a) match {
      case Point(0, y)            => rangeZeroTo(y).map(n => a.add(0, n))
      case Point(x, 0)            => rangeZeroTo(x).map(n => a.add(n, 0))
      case Point(x, y) if x == y  => rangeZeroTo(x).map(n => a.add(n, n))
      case Point(x, y) if x == -y => rangeZeroTo(x).map(n => a.add(n, -n))
    }
  }
}

def parsePoint(in: String): Point = in
  .split(",")
  .map(_.toInt)
  .toList match {
  case x :: y :: _ => Point(x, y)
  case _ => throw RuntimeException(s"Unable to pars point given '$in'.")
}

def parseLine(in: String): Line = in
  .split(" -> ")
  .toList match {
  case a :: b :: Nil => Line(parsePoint(a), parsePoint(b))
  case _ => throw RuntimeException(s"Unable to pars line given '$in'.")
}

@main def main(filePath: String, part: Int) = {
  println(s"Day 5 (Hydrothermal Venture) part $part using file '$filePath'")
  val textLines = scala.io.Source.fromFile(filePath, "UTF-8").getLines.toList

  val allLines = textLines
    .map(parseLine)
    .filter(l => l.isVerticalOrHorizontal || (part == 2 && l.isDiagonal))

  val allPoints = allLines.flatMap(_.allPointsOnLine)
  val uniqueMultioccurencePoints = allPoints.diff(allPoints.distinct).distinct
  println(uniqueMultioccurencePoints.size)
}
