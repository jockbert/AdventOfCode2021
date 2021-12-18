import scala.annotation.tailrec

def isDigitChar(x: Char) = '0' <= x && x <= '9'

sealed trait Tree { def magnitude: BigInt };
case class Node(var left: Tree, var right: Tree) extends Tree {
  def magnitude = left.magnitude * 3 + right.magnitude * 2
  override def toString() = s"[$left,$right]"
}

case class Leaf(var value: Int) extends Tree {
  def magnitude = value; override def toString() = s"$value"
}

type Chars = List[Char]
type Tokens = List[Token]
sealed trait Token;
case class LeftBrace() extends Token {
  override def toString() = "["
}
case class RightBrace() extends Token {
  override def toString() = "]"
}
case class Value(v: BigInt) extends Token {
  override def toString() = v.toString()
}

@tailrec def parseNum(text: Chars, res: BigInt = 0): (BigInt, Chars) = {
  text match {
    case Nil                      => (res, Nil)
    case x :: _ if isDigitChar(x) => parseNum(text.tail, res * 10 + x.asDigit)
    case _                        => (res, text)
  }
}

def parseTokens(text: String): Tokens = {
  def p(cs: Chars, res: Tokens = Nil): Tokens =
    cs match {
      case Nil      => res.reverse
      case '[' :: _ => p(cs.tail, LeftBrace() :: res)
      case ',' :: _ => p(cs.tail, res)
      case ']' :: _ => p(cs.tail, RightBrace() :: res)
      case n :: _ if isDigitChar(n) =>
        val (v, cs2) = parseNum(cs); p(cs2, Value(v) :: res)
      case x => throw Exception(s"Error on char '$x' when parsing tokens")
    }
  p(text.toList)
}

/** Add value to the first Value-token found in list. */
def addToFirst(addition: BigInt, ts: Tokens): Tokens = {
  ts match {
    case Nil              => Nil
    case Value(x) :: rest => Value(x + addition) :: rest
    case head :: tail     => head :: addToFirst(addition, tail)
  }
}

@tailrec
def explode(fwd: Tokens, bwd: Tokens = Nil, depth: Int = 0): Option[Tokens] =
  fwd match {
    case Nil => None
    case LeftBrace() :: Value(a) :: Value(b) :: RightBrace() :: rest
        if depth >= 4 =>
      Some(addToFirst(a, bwd).reverse ::: Value(0) :: addToFirst(b, rest))
    case LeftBrace() :: _  => explode(fwd.tail, fwd.head :: bwd, depth + 1)
    case RightBrace() :: _ => explode(fwd.tail, fwd.head :: bwd, depth - 1)
    case _                 => explode(fwd.tail, fwd.head :: bwd, depth)
  }

@tailrec
def split(fwd: Tokens, bwd: Tokens = Nil): Option[Tokens] =
  fwd match {
    case Nil => None
    case Value(v) :: _ if v > 9 =>
      Some(
        bwd.reverse ::: LeftBrace() :: Value(v / 2) ::
          Value((v + 1) / 2) :: RightBrace() :: fwd.tail
      )
    case LeftBrace() :: _  => split(fwd.tail, fwd.head :: bwd)
    case RightBrace() :: _ => split(fwd.tail, fwd.head :: bwd)
    case Value(_) :: _     => split(fwd.tail, fwd.head :: bwd)
  }

@tailrec
def reduce(ts: Tokens): Tokens = {
  val changedTs = explode(ts).orElse(split(ts))
  if (changedTs.isEmpty) ts else reduce(changedTs.get)
}

def add(l: Tokens, r: Tokens): Tokens =
  reduce(LeftBrace() :: l ::: r ::: (RightBrace() :: Nil))

def treeFromTokens(tokens: Tokens): Tree = {
  def tft(ts: Tokens): (Tree, Tokens) = ts match {
    case Nil                  => throw Exception("Unexpected tokens end")
    case Value(v) :: rest     => (Leaf(v.toInt), rest)
    case RightBrace() :: rest => tft(rest)
    case LeftBrace() :: rest =>
      val (leftSubTree, rest2) = tft(rest)
      val (rightSubTree, rest3) = tft(rest2)
      (Node(leftSubTree, rightSubTree), rest3)
  }
  tft(tokens)._1
}

def toString(ts: Tokens) = treeFromTokens(ts).toString
def magnitude(ts: Tokens) = treeFromTokens(ts).magnitude

@main def main(filePath: String) = {
  println(s"Day 18 (Snailfish) using file '$filePath'")
  val textLines = scala.io.Source.fromFile(filePath, "UTF-8").getLines.toList
  val numbers: List[Tokens] = textLines.map(parseTokens)
  val sum: Tokens = numbers.tail.foldLeft(numbers.head)(add(_, _))

  println("Part 1, sum of all values:  " + magnitude(sum))

  val maxSum = numbers
    .flatMap(a => numbers.filterNot(a.equals).map(b => magnitude(add(b, a))))
    .max

  println("Part 2, max two number sum: " + maxSum)
}
