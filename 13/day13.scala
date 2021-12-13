case class Dot(x: Int, y: Int) {
  def foldX(crease: Int) = if (x > crease) Dot(2 * crease - x, y) else this
  def foldY(crease: Int) = if (y > crease) Dot(x, 2 * crease - y) else this
  def max(other: Dot) = Dot(Math.max(x, other.x), Math.max(y, other.y))
}

case class Fold(crease: Int, isAlongX: Boolean) {
  def apply(dot: Dot): Dot =
    if (isAlongX) dot.foldX(crease) else dot.foldY(crease)
}

@main def main(filePath: String) = {
  println(s"Day 13 (Transparent Origami) using file '$filePath'")
  val textLines = scala.io.Source.fromFile(filePath, "UTF-8").getLines.toList
  val dots =
    textLines
      .takeWhile(!_.isBlank)
      .map(_.split(","))
      .map(a => Dot(a(0).toInt, a(1).toInt))
      .toSet

  val folds = textLines
    .dropWhile(!_.contains("fold"))
    .map(_.split("="))
    .map(a => Fold(a(1).toInt, a(0).contains("x")))

  val firstFoldDots = dots.map(folds.head.apply)
  println("part 1: " + firstFoldDots.size)

  val finalDots = folds.foldLeft(dots)((ds, fold) => ds.map(fold.apply))
  val max = finalDots.fold(Dot(0, 0))((a, b) => a.max(b))
  val graph = (-1 to max.y + 1)
    .map(y =>
      (-1 to max.x + 1)
        .map(x => if (finalDots.contains(Dot(x, y))) "█" else " ")
        .mkString
    )
    .mkString("\n")

  println()
  println("part 2:\n" + graph)
}
