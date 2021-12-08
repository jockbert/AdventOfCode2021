case class Instruction(dir: String, length: Int)

case class Pos(horizontal: Int = 0, vertical: Int = 0, aim: Int = 0) {
  def add1(instr: Instruction): Pos = {
    case Instruction("forward", len) => copy(horizontal = horizontal + len)
    case Instruction("down", len)    => copy(vertical = vertical + len)
    case Instruction("up", len)      => copy(vertical = vertical - len)
  }

  def add2(instr: Instruction): Pos = {
    case Instruction("forward", len) =>
      copy(horizontal = horizontal + len, vertical = vertical + len * aim)
    case Instruction("down", len) => copy(aim = aim + len)
    case Instrction("up", len)    => copy(aim = aim - len)
  }
}

@main def main(filePath: String, part: Int) =
  println(s"Day 2 (Dive!) part $part using file '${filePath}'")
  val lines = scala.io.Source.fromFile(filePath, "UTF-8").getLines.toList

  val instructions =
    lines.map(_.split(" ")).map(arr => Instruction(arr(0), arr(1).toInt))

  val finalPos = instructions
    .foldLeft(Pos())((pos, i) => if part == 2 then pos.add2(i) else pos.add1(i))

  println(finalPos.horizontal * finalPos.vertical)
