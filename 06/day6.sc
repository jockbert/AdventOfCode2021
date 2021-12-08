import scala.annotation.tailrec

case class Population(
    t0: BigInt,
    t1: BigInt,
    t2: BigInt,
    t3: BigInt,
    t4: BigInt,
    t5: BigInt,
    t6: BigInt,
    t7: BigInt,
    t8: BigInt
) {
  def advance = Population(t1, t2, t3, t4, t5, t6, t7 + t0, t8, t0)
  def count = t0 + t1 + t2 + t3 + t4 + t5 + t6 + t7 + t8

  @tailrec final def simulate(days: Int): Population =
    if (days <= 0) this else advance.simulate(days - 1)
}

@main def main(filePath: String, part: Int) = {
  println(s"AOC day 6 (Lanternfish) part $part using file '$filePath'")
  val textLines = scala.io.Source.fromFile(filePath, "UTF-8").getLines.toList
  val individuals: List[Int] = textLines.head.split(",").map(_.toInt).toList
  val countPerTimer = individuals.groupMapReduce(n => n)(_ => 1)(_ + _)

  val initPop = Population(
    countPerTimer.getOrElse(0, 0),
    countPerTimer.getOrElse(1, 0),
    countPerTimer.getOrElse(2, 0),
    countPerTimer.getOrElse(3, 0),
    countPerTimer.getOrElse(4, 0),
    countPerTimer.getOrElse(5, 0),
    countPerTimer.getOrElse(6, 0),
    countPerTimer.getOrElse(7, 0),
    countPerTimer.getOrElse(8, 0)
  )

  var finalPop = initPop.simulate(if part == 2 then 256 else 80)

  println(finalPop.count)
}
