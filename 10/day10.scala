import scala.annotation.tailrec

sealed trait ParseError {
  def isCorrupt: Boolean
  def isIncomplete = !isCorrupt
  def score: BigInt
}

case class Corrupt(errorChar: Char) extends ParseError {
  def isCorrupt = true
  def score = errorChar match
    case ')' => 3
    case ']' => 57
    case '}' => 1197
    case '>' => 25137
}

case class Incomplete(missingEnd: String) extends ParseError {
  def isCorrupt = false
  def score = missingEnd.foldLeft(BigInt(0))((sum, ch) =>
    sum * 5 + (ch match
      case ')' => 1
      case ']' => 2
      case '}' => 3
      case '>' => 4
    )
  )
}

def findParseError(line: String): ParseError = {
  def throwRE(message: String) = throw new RuntimeException(message)

  @tailrec def parse(in: List[Char], endStack: List[Char] = Nil): ParseError = {
    val stack2 = (in, endStack) match {
      case (Nil, Nil)    => throwRE(s"Unexpectedly corret line $line")
      case (Nil, s :: _) => return Incomplete(endStack.mkString)
      case ('(' :: _, _) => ')' :: endStack
      case ('[' :: _, _) => ']' :: endStack
      case ('{' :: _, _) => '}' :: endStack
      case ('<' :: _, _) => '>' :: endStack
      case (i :: _, s :: _) if i != s => return Corrupt(i)
      case (i :: _, Nil)              => return Corrupt(i)
      case (i :: _, s :: sRest)       => sRest
    }
    parse(in.tail, stack2)
  }
  parse(line.toList)
}

@main def main(filePath: String) = {
  println(s"Day 10 (Syntax Scoring) using file '$filePath'")
  val textLines = scala.io.Source.fromFile(filePath, "UTF-8").getLines.toList
  val parseErrors = textLines.map(findParseError)
  println("part 1: " + parseErrors.filter(_.isCorrupt).map(_.score).sum)

  val scores = parseErrors.filter(_.isIncomplete).map(_.score).sorted
  println("part 2: " + scores.drop(scores.size / 2).head)
}
