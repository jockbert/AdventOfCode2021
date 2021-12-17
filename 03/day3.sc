case class Sum(ones: List[Int] = Nil) {

  def add(line: String): Sum = {
    val bits = line.toSeq.map(c => if c == '1' then 1 else -1)

    val newOnes = ones
      .zipAll(bits, 0, 0)
      .map((o, b) => o + b)
      .toList

    Sum(newOnes)
  }

  def gamma(): Int = ones
    .map(i => if i > 0 then 1 else 0)
    .foldLeft(0)((acc, bit) => (acc << 1) + bit)

  def epsilon(): Int = ones
    .map(i => if i > 0 then 0 else 1)
    .foldLeft(0)((acc, bit) => (acc << 1) + bit)
}

@main def main(filePath: String) = {
  println(s"Day 3 (Binary Diagnostics) using file '$filePath'")
  val textLines = scala.io.Source.fromFile(filePath, "UTF-8").getLines.toList
  val s: Sum = textLines.foldLeft(Sum())((sum, line) => sum.add(line))
  println("part 1: " + s.gamma() * s.epsilon())
}
