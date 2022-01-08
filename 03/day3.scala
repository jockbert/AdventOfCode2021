case class Sum(ones: List[Int] = Nil, lines: List[String] = Nil) {

  def add(line: String): Sum = {
    val bits = line.toSeq.map(c => if c == '1' then 1 else -1)

    val newOnes = ones
      .zipAll(bits, 0, 0)
      .map((o, b) => o + b)
      .toList

    Sum(newOnes, line :: lines)
  }

  def addAll(lines: List[String]): Sum =
    lines.foldLeft(this)(_.add(_))

  def gamma(): Int = ones
    .map(i => if i > 0 then 1 else 0)
    .foldLeft(0)((acc, bit) => (acc << 1) + bit)

  def epsilon(): Int = ones
    .map(i => if i > 0 then 0 else 1)
    .foldLeft(0)((acc, bit) => (acc << 1) + bit)

  def oxygenGenRating(index: Int = 0): Int = {
    if (lines.size == 1) then Integer.parseInt(lines.head, 2)
    else
      val searchedBit = if ones.drop(index).head >= 0 then '1' else '0'
      val filteredLines = lines.filter(_.drop(index).head == searchedBit)
      Sum().addAll(filteredLines).oxygenGenRating(index + 1)
  }

  def co2ScrubberRating(index: Int = 0): Int = {
    if (lines.size == 1) then Integer.parseInt(lines.head, 2)
    else
      val searchedBit = if ones.drop(index).head >= 0 then '0' else '1'
      val filteredLines = lines.filter(_.drop(index).head == searchedBit)
      Sum().addAll(filteredLines).co2ScrubberRating(index + 1)
  }
}

@main def main(filePath: String) = {
  println(s"Day 3 (Binary Diagnostics) using file '$filePath'")
  val textLines = scala.io.Source.fromFile(filePath, "UTF-8").getLines.toList
  val s: Sum = textLines.foldLeft(Sum())((sum, line) => sum.add(line))
  println("part 1: " + s.gamma() * s.epsilon())
  println("part 2: " + s.oxygenGenRating() * s.co2ScrubberRating())
}
