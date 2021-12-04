case class Instruction(dir: String, length: Int)

case class Pos(horizontal: Int = 0, vertical: Int = 0, aim: Int = 0) {

  def add(instr: Instruction): Pos = {
    instr.dir match {
      case "forward" =>
        copy(
          horizontal = horizontal + instr.length,
          vertical = vertical + instr.length * aim
        )
      case "down" => copy(aim = aim + instr.length)
      case "up"   => copy(aim = aim - instr.length)
    }
  }
}

@main def main(filePath: String) =
  println(s"AOC day 2 part 2, with file '${filePath}'")
  val lines = scala.io.Source.fromFile(filePath, "UTF-8").getLines.toList

  val instructions = lines
    .map(_.split(" "))
    .map(arr => Instruction(arr(0), arr(1).toInt))
    .toList

  val finalPos = instructions
    .foldLeft(Pos())((pos, instr) => pos.add(instr))

  println(finalPos.horizontal * finalPos.vertical)
