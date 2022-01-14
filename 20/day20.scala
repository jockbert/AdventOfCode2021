case class Point(x: Int, y: Int) {
  def +(other: Point) = Point(x + other.x, y + other.y)
  def area = x * y
  def allPointsWithin() =
    (0 until y).flatMap(yy => (0 until x).map(xx => Point(xx, yy)))
}

case class Image(
    data: Set[Point],
    min: Point,
    max: Point,
    isInfinityLit: Boolean
) {
  def isInImage(p: Point) =
    p.x >= min.x && p.x <= max.x && p.y >= min.y && p.y <= max.y

  def isLitUp(p: Point) =
    if isInImage(p) then data.contains(p) else isInfinityLit

  def allPointsBetween(min: Point, max: Point) = (min.y to max.y)
    .flatMap(y => (min.x to max.x).map(x => Point(x, y)))
    .toSeq

  def enhance(lookup: Vector[Boolean]): Image = {
    val newMin = min + Point(-1, -1)
    val newMax = max + Point(1, 1)

    val adjacent =
      Point(-1, -1) :: Point(0, -1) :: Point(1, -1) ::
        Point(-1, 0) :: Point(0, 0) :: Point(1, 0) ::
        Point(-1, 1) :: Point(0, 1) :: Point(1, 1) :: Nil

    def index(p: Point) = adjacent
      .map(_ + p)
      .foldLeft(0)((acc, p) => (acc << 1) + (if isLitUp(p) then 1 else 0))

    def isLitUpWhenEnhanced(p: Point): Boolean = lookup(index(p))
    def allPointsInNewImage = allPointsBetween(newMin, newMax)

    val isNewInfinityLit = lookup(if isInfinityLit then 511 else 0)

    Image(
      allPointsInNewImage.filter(isLitUpWhenEnhanced).toSet,
      newMin,
      newMax,
      isNewInfinityLit
    )
  }

  override def toString() = (min.y to max.y)
    .map(y =>
      (min.x to max.x)
        .map(x => data.contains(Point(x, y)))
        .map(if _ then "#" else ".")
        .mkString
    )
    .mkString("\n")
}

@main def main(filePath: String) =
  println(s"Day 20 (Trench Map) using file '$filePath'")
  val textLines = scala.io.Source.fromFile(filePath, "UTF-8").getLines.toList
  val lookup: Vector[Boolean] = textLines.head.map(_.equals('#')).toVector
  val input = textLines.tail.tail.map(_.map(_.equals('#')).toVector).toVector
  val size = Point(input(0).size, input.size)
  val data = size.allPointsWithin().filter(p => input(p.y)(p.x)).toSet

  val original = Image(data, Point(0, 0), size + Point(-1, -1), false)
  val image1 = original.enhance(lookup)
  val image2 = image1.enhance(lookup)
  val image50 = (1 to 50).foldLeft(original)((img, _) => img.enhance(lookup))

  println("\n" + original)
  println("\n" + image1)
  println("\n" + image2)
  println("\nPart 1 - lit pixel count after 2 loops: " + image2.data.size)

  println("\n" + image50)
  println("\nPart 2 - lit pixel count after 50 loops: " + image50.data.size)
