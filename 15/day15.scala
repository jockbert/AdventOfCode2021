import scala.annotation.tailrec
import java.lang.Math.{min, max}

case class Point(x: Int, y: Int) {
  def up = Point(x, y - 1)
  def left = Point(x - 1, y)
  def area = x * y
  def allPointsWithin() =
    (0 until y).flatMap(yy => (0 until x).map(xx => Point(xx, yy)))
}
object Matrix {
  def apply(size: Point, value: Int): Matrix =
    Matrix(
      (1 to size.y).toArray
        .map(y =>
          (1 to size.x).toArray
            .map(x => value)
        )
    )
}

case class Matrix(data: Array[Array[Int]]) {
  def size() = Point(data(0).size, data.size)
  def apply(p: Point) = data(p.y)(p.x)
  def apply(p: Point, v: Int): Unit = data(p.y)(p.x) = v
  def mirror(): Matrix = Matrix(data.map(_.reverse).reverse)

  def isSameData(other: Matrix) =
    size().allPointsWithin().forall(p => this(p) == other(p))
}

def print(title: String, m: Matrix) = {
  println(title + ":")
  println(
    m.data
      .map(row =>
        "\t" + row.map(_.toString.reverse.padTo(4, ' ').reverse).mkString
      )
      .mkString("\n")
  )
}

def dpCosts(levels: Matrix, costs: Matrix): Matrix = {
  val result = Matrix(levels.size(), -1)
  levels
    .size()
    .allPointsWithin()
    .map(p =>
      p match {
        case Point(0, 0) => result(p, costs(p))
        case Point(0, _) =>
          result(p, min(costs(p), result(p.up) + levels(p)))
        case Point(_, 0) =>
          result(p, min(costs(p), result(p.left) + levels(p)))
        case _ =>
          result(
            p,
            min(costs(p), min(result(p.left), result(p.up)) + levels(p))
          )
      }
    )
  result
}

def dpOmnidirCosts(levels: Matrix, costs: Matrix): Matrix = {
  val costs2 = dpCosts(levels, costs)
  dpCosts(levels.mirror(), costs2.mirror()).mirror()
}

@tailrec def iterateUntilSame(riskLevels: Matrix, costs: Matrix): Matrix = {
  System.out.print(".")
  val newCosts = dpOmnidirCosts(riskLevels, costs)
  if (newCosts.isSameData(costs)) newCosts
  else iterateUntilSame(riskLevels, newCosts)
}

def lowestTotalRisk(riskLevels: Matrix): Int = {
  // create initial costs larger than max cost nad set cost 0 in start position
  val maxValue = riskLevels.size().area * 9
  val costs = Matrix(riskLevels.size(), maxValue)
  costs(Point(0, 0), 0)

  val end = riskLevels.size().up.left
  System.out.print(" iterating solution ")
  val fin = iterateUntilSame(riskLevels, costs)
  println()
  fin(end)
}

@main def main(filePath: String) = {
  println(s"Day 15 (Chitons) using file '$filePath'")
  val textLines = scala.io.Source.fromFile(filePath, "UTF-8").getLines.toList
  val data = textLines.map(_.map(_.asDigit).toArray).toArray

  println("part 1: " + lowestTotalRisk(Matrix(data)))

  // Enlarge risk levels 5 times in both dimensions
  def inc(a: Int, b: Int) = { val n = a + b; if n > 9 then n - 9 else n }
  val data5: Array[Array[Int]] =
    data.map(line => (0 to 4).flatMap(x => line.map(n => inc(n, x))).toArray)
  val data25 =
    (0 to 4).toArray.flatMap(x => data5.map(line => line.map(n => inc(n, x))))

  println("part 2: " + lowestTotalRisk(Matrix(data25)))
}
