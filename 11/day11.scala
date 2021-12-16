import scala.annotation.tailrec

case class Point(x: Int, y: Int) {
  def +(other: Point) = Point(x + other.x, y + other.y)
  def all = (0 until y).flatMap(b => (0 until x).map(a => Point(a, b)))
  def area = x * y
}

case class Dumbos(arr: Array[Array[Int]]) {
  override def toString(): String = arr
    .map(_.map(l => if (l == 10) "*" else if (l > 10) "." else l).mkString)
    .mkString("Dumbos(", "\n       ", ")")

  val size = Point(arr(0).size, arr.size)
  def level(p: Point): Int = arr(p.y)(p.x)
  def level(p: Point, lvlFn: Int => Int): Unit = arr(p.y)(p.x) = lvlFn(level(p))
  def resetLevels() = size.all.foreach(p => level(p, l => if (l > 9) 0 else l))
  def adjacentTo(p: Point) =
    List((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))
      .map(Point.apply.tupled)
      .map(_ + p)
      .filter({
        case Point(-1, _) | Point(_, -1)         => false
        case Point(size.x, _) | Point(_, size.y) => false
        case _                                   => true
      })

  def step(): Int =
    @tailrec def loop(toBeExcited: Seq[Point], flashCount: Int): Int =
      if (toBeExcited.isEmpty) { resetLevels(); flashCount }
      else {
        val p1 = toBeExcited.head
        level(p1, _ + 1)
        val isFlashing = level(p1) == 10
        val newFlashes = if isFlashing then 1 else 0
        val newExcitements = if isFlashing then adjacentTo(p1) else Seq.empty
        loop(toBeExcited.tail ++ newExcitements, flashCount + newFlashes)
      }
    loop(size.all, 0)
}

def countFlashes(ds: Dumbos, steps: Int) =
  (0 until steps).map(_ => ds.step()).sum

def findSyncStep(ds: Dumbos) =
  LazyList.from(1).dropWhile(_ => ds.step() != ds.size.area).head

@main def main(filePath: String) =
  println(s"Day 11 (Dumbo Octopus) using file '$filePath'")
  val textLines = scala.io.Source.fromFile(filePath, "UTF-8").getLines.toArray
  val ds = Dumbos(textLines.map(_.map(_.asDigit).toArray))
  println("part 1: " + countFlashes(ds, 100))
  println("part 2: " + findSyncStep(ds))
