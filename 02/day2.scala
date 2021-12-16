case class Instr(dir: String, length: Int)

case class Pos(horizontal: Int = 0, vertical: Int = 0, aim: Int = 0) {

  def add1(instr: Instr): Pos = instr match {
    case Instr("forward", len) => copy(horizontal = horizontal + len)
    case Instr("down", len)    => copy(vertical = vertical + len)
    case Instr("up", len)      => copy(vertical = vertical - len)
  }

  def add2(instr: Instr): Pos = instr match {
    case Instr("forward", len) =>
      copy(horizontal = horizontal + len, vertical = vertical + len * aim)
    case Instr("down", len) => copy(aim = aim + len)
    case Instr("up", len)   => copy(aim = aim - len)
  }

  def product = horizontal * vertical
}

@main def main(filePath: String) =
  println(s"Day 2 (Dive!) using file '${filePath}'")
  val lines = scala.io.Source.fromFile(filePath, "UTF-8").getLines.toList
  val instrs = lines.map(_.split(" ")).map(arr => Instr(arr(0), arr(1).toInt))

  println("part 1: " + instrs.foldLeft(Pos())((pos, i) => pos.add1(i)).product)
  println("part 2: " + instrs.foldLeft(Pos())((pos, i) => pos.add2(i)).product)
