import scala.annotation.tailrec

case class Point(x: Int, y: Int) {

  def adjacents(size: Point): List[Point] = {
    val p = Point.apply
    List(p(x - 1, y), p(x + 1, y), p(x, y - 1), p(x, y + 1)).filter({
      case Point(-1, _)     => false
      case Point(_, -1)     => false
      case Point(size.x, _) => false
      case Point(_, size.y) => false
      case _                => true
    })
  }

  def allPointsWithin =
    (0 until y).flatMap(yy => (0 until x).map(xx => Point(xx, yy)))
}

case class Board(data: Array[Array[Int]]) {
  val size = Point(data(0).length, data.length)
  def value(p: Point) = data(p.y)(p.x)
  override def toString() =
    data.foldLeft("Board(")(_ + "\n" + _.foldLeft("\t")(_ + _)) + ")"
}

def isLowest(board: Board)(p: Point): Boolean =
  p.adjacents(board.size).forall(a => board.value(a) > board.value(p))

def findBaisin(board: Board)(lowest: Point): Set[Point] = {

  @tailrec def expand(unvisited: Set[Point], visited: Set[Point]): Set[Point] =
    if unvisited.isEmpty then visited
    else {
      val p = unvisited.head
      val candidates = p
        .adjacents(board.size)
        .filterNot(visited.contains)
        .filterNot(a => board.value(a) == 9)
      expand(unvisited ++ candidates - p, visited + p)
    }

  expand(Set(lowest), Set())
}

@main def main(filePath: String, part: Int) = {
  println(s"Day 9 (Smoke Basin) part $part using file '$filePath'")
  val textLines = scala.io.Source.fromFile(filePath, "UTF-8").getLines.toList

  // 48 is offset to ASCII '0'
  val board = Board(textLines.map(_.toArray.map(c => (c - 48).toInt)).toArray)
  val lowestPoints = board.size.allPointsWithin.filter(isLowest(board))

  if part == 2 then
    val allBasins = lowestPoints.map(findBaisin(board))
    val top3BasinSizes = allBasins.map(_.size).sorted.reverse.take(3)
    println(top3BasinSizes.product)
  else
    val riskLevelSum = lowestPoints.map(p => board.value(p) + 1).sum
    println(riskLevelSum)
  end if
}
