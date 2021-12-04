case class P(x: Int, y: Int) {
  def sub(other: P) = P(x - other.x, y - other.y)
  def add(ox: Int, oy: Int) = P(x + ox, y + oy)
}

case class Line(a: P, b: P) {

  def isVerticalOrHorizontal = a.x == b.x || a.y == b.y
  def isDiagonal = a.sub(b) match {
    case P(n, m) if n == m  => true
    case P(n, m) if n == -m => true
    case _                  => false
  }

  def allPointsAlong: List[P] = {
    def rangeZeroTo(n: Int) = (0 to n by n.sign).toList
    b.sub(a) match {
      case P(0, y)            => rangeZeroTo(y).map(n => a.add(0, n))
      case P(x, 0)            => rangeZeroTo(x).map(n => a.add(n, 0))
      case P(x, y) if x == y  => rangeZeroTo(x).map(n => a.add(n, n))
      case P(x, y) if x == -y => rangeZeroTo(x).map(n => a.add(n, -n))
    }
  }
}

def parsePoint(in: String): P = {
  in.split(",").map(_.toInt).toList match {
    case x :: y :: _ => P(x, y)
    case _ => throw RuntimeException(s"Unable to pars point given '$in'.")
  }
}

def parseLine(in: String): Line = {
  in.split(" -> ").toList match {
    case a :: b :: Nil => Line(parsePoint(a), parsePoint(b))
    case _ => throw RuntimeException(s"Unable to pars point given '$in'.")
  }
}

@main def main(filePath: String, part: Int) = {
  val isPart2 = part.equals(2)
  println(s"AOC day 5 part $part, with file '$filePath'")
  val textLines = scala.io.Source.fromFile(filePath, "UTF-8").getLines.toList

  val allLines = textLines
    .map(parseLine)
    .filter(l => l.isVerticalOrHorizontal || (isPart2 && l.isDiagonal))

  val allPoints = allLines.flatMap(line => line.allPointsAlong)
  val uniqueMultioccurencePoints = allPoints.diff(allPoints.distinct).distinct
  println(uniqueMultioccurencePoints.size)
}
