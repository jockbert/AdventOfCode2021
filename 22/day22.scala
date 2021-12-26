import scala.annotation.tailrec

/** Range in N dimensions with a start and an end (inclusive). */
trait Range[T <: Range[T]] {
  def self: T
  def isSubsetOf(o: T): Boolean
  def intersect(o: T): Option[T]
  def inverse(): List[T]
  def size: BigInt
  def all: T

  def diff(other: T): List[T] =
    if isSubsetOf(other) then Nil
    else
      this
        .intersect(other)
        .map(_.inverse().flatMap(_.intersect(self)))
        .getOrElse(List(self))

  def union(other: T): List[T] =
    if isSubsetOf(other) then List(other)
    else if other.isSubsetOf(self) then List(self)
    else intersect(other).map(_ :: diff(other)).getOrElse(List(self, other))
}

/** 1D line range */
case class Line(start: Int, end: Int) extends Range[Line] {
  def min = Int.MinValue; def max = Int.MaxValue
  def isSubsetOf(other: Line) = start >= other.start && end <= other.end
  def intersect(other: Line) =
    if start <= other.end && other.start <= end then
      Some(Line(Math.max(start, other.start), Math.min(end, other.end)))
    else None
  def inverse() =
    if start == min && end == max then Nil
    else if start == min then Line(end + 1, max) :: Nil
    else if end == max then Line(min, start - 1) :: Nil
    else Line(min, start - 1) :: Line(end + 1, max) :: Nil
  def size = end - start + 1
  def all = Line(min, max)
  def self = this
}

/** Range with extra Line dimension added to it. */
trait ExtraDimRange[B <: Range[B], C <: ExtraDimRange[B, C]] extends Range[C] {

  /** Get inner Line range component A */
  def a: Line

  /** Get inner range component B */
  def b: B

  /** Crete new range of type C */
  def c(a: Line, b: B): C

  def isSubsetOf(o: C) = a.isSubsetOf(o.a) && b.isSubsetOf(o.b)
  def intersect(o: C) =
    a.intersect(o.a).flatMap(ai => b.intersect(o.b).map(bi => c(ai, bi)))
  def inverse() =
    a.inverse().map(ai => c(ai, b.all)) ::: b.inverse().map(bi => c(a, bi))
  def size = a.size * b.size
  def all = c(a.all, b.all)
}

/** 2D area range */
case class Area(x: Line, y: Line) extends ExtraDimRange[Line, Area] {
  def a = x; def b = y; def c(m: Line, n: Line) = Area(m, n); def self = this
}

/** 3D block range */
case class Block(x: Line, y: Line, z: Line) extends ExtraDimRange[Area, Block] {
  def a = x; def b = Area(y, z); def c(m: Line, n: Area) = Block(m, n.x, n.y);
  def self = this
}

enum State:
  case On, Off

case class Instruction(state: State, range: Block)

def parseInstruction(textLine: String) = {
  val state = if (textLine.startsWith("on")) State.On else State.Off
  val ranges = textLine
    .split(",")
    .map(_.split("=")(1).split("\\.\\."))
    .map(arr => Line(arr(0).toInt, arr(1).toInt))
  Instruction(state, Block(ranges(0), ranges(1), ranges(2)))
}

def addToRanges(ranges: List[Block], additional: Block): List[Block] =
  additional :: removeFromRanges(ranges, additional)

def removeFromRanges(ranges: List[Block], removal: Block): List[Block] =
  ranges.flatMap(r => r.diff(removal))

@main def main(filePath: String) = {
  println(s"Day 22 (Reactor Reboot) using file '$filePath'")
  val textLines = scala.io.Source.fromFile(filePath, "UTF-8").getLines.toList
  val instructions = textLines.map(parseInstruction)
  val boundingBox = Block(Line(-50, 50), Line(-50, 50), Line(-50, 50))

  val lightUps = instructions.foldLeft(List.empty[Block])((lightUps, instr) =>
    if instr.state == State.On then addToRanges(lightUps, instr.range)
    else removeFromRanges(lightUps, instr.range)
  )

  val lightUpCount1 = lightUps.flatMap(_.intersect(boundingBox)).map(_.size).sum
  println("Part 1, light up cubes in -50..50 bounding box: " + lightUpCount1)

  val lightUpCount2 = lightUps.map(_.size).sum
  println("Part 2, light up cubes all-in-all: " + lightUpCount2)
}
