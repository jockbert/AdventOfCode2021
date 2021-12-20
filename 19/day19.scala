import scala.annotation.tailrec

/** Vector */
case class Vec(x: Int, y: Int, z: Int) {
  def neg() = Vec(-x, -y, -z)
  def +(o: Vec) = Vec(x + o.x, y + o.y, z + o.z)
  def -(o: Vec) = this + o.neg()
  def abs(o: Vec) = {
    val d = this - o; Vec(Math.abs(d.x), Math.abs(d.y), Math.abs(d.z))
  }
  def manhattan(o: Vec) = { val a = abs(o); a.x + a.y + a.z }
  override def toString() = s"[$x,$y,$z]"
}

/** Rotation matrix */
case class Rot(col1: Vec, col2: Vec, col3: Vec) {
  def *(o: Rot): Rot = Rot(this * o.col1, this * o.col2, this * o.col3)
  def *(o: Vec): Vec = Vec(
    o.x * col1.x + o.y * col2.x + o.z * col3.x,
    o.x * col1.y + o.y * col2.y + o.z * col3.y,
    o.x * col1.z + o.y * col2.z + o.z * col3.z
  )
  override def toString() = s"[$col1,$col2,$col3]"
}

def allRotations = {
  val rot0 = Rot(Vec(1, 0, 0), Vec(0, 1, 0), Vec(0, 0, 1))
  val rotX = Rot(Vec(1, 0, 0), Vec(0, 0, 1), Vec(0, -1, 0))
  val rotY = Rot(Vec(0, 0, -1), Vec(0, 1, 0), Vec(1, 0, 0))
  val rotZ = Rot(Vec(0, 1, 0), Vec(-1, 0, 0), Vec(0, 0, 1))
  val allRotZ = rot0 :: rotZ :: rotZ * rotZ :: rotZ * rotZ * rotZ :: Nil

  val allFacesZ = rot0 :: rotX :: rotX * rotX :: rotX * rotX * rotX ::
    rotY :: rotY * rotY * rotY :: Nil

  allFacesZ.flatMap(r1 => allRotZ.map(r2 => r2 * r1))
}

case class System(
    beacons: List[Vec] = Nil,
    scanners: List[Vec] = Vec(0, 0, 0) :: Nil
) {

  def addBeacon(v: Vec) = System(v :: beacons, scanners)
  def rotate(rot: Rot) = System(beacons.map(rot * _), scanners.map(rot * _))
  def translate(trans: Vec) =
    System(beacons.map(trans + _), scanners.map(trans + _))
  def merge(other: System) = System(
    beacons.appendedAll(other.beacons).distinct,
    scanners.appendedAll(other.scanners).distinct
  )

  override def toString(): String =
    "System\n\tbeacons\n\t\t" + beacons.mkString("\n\t\t")
      + "\n\tscanners\n\t\t" + scanners.mkString("\n\t\t")
}

type Systems = List[System]

@tailrec def parseLines(
    lines: List[String],
    res: Systems = Nil
): Systems =
  lines match {
    case Nil                 => res.reverse
    case s :: _ if s.isBlank => parseLines(lines.tail, res)
    case s :: _ if s.contains("--- scanner") =>
      parseLines(lines.tail, System() :: res)
    case s :: _ if s.contains(",") => {
      val ints = s.split(",").map(_.toInt)
      val bcn = Vec(ints(0), ints(1), ints(2))
      parseLines(lines.tail, res.head.addBeacon(bcn) :: res.tail)
    }
    case x :: _ => throw Exception(s"Unable to parse line $x")
  }

def tryMerge(a: System, b: System): Systems = {
  val maybeMatch = (for {
    rot <- allRotations
    b2 = b.rotate(rot)
    aBcn <- a.beacons.drop(11)
    bBcn <- b2.beacons.drop(11)
    b3 = b2.translate(aBcn - bBcn)
    if b3.beacons.intersect(a.beacons).size >= 12
  } yield b3).headOption

  maybeMatch match {
    case Some(bSys) => a.merge(bSys) :: Nil
    case None       => a :: b :: Nil
  }
}

def tryMerge(systems: Systems): Systems = {
  systems match {
    case Nil      => systems
    case s :: Nil => systems
    case a :: bs =>
      val res =
        bs.foldLeft(a :: Nil)((acc, b) => tryMerge(acc.head, b) ::: acc.tail)
      res.head :: tryMerge(res.tail)
  }
}

@tailrec def mergeSystems(systems: Systems): System = systems match {
  case s :: Nil => s
  case _        => mergeSystems(tryMerge(systems))
}

@main def main(filePath: String) = {
  println(s"Day 19 (Beacon Scanner) using file '$filePath'")
  val textLines = scala.io.Source.fromFile(filePath, "UTF-8").getLines.toList
  val allSystems = parseLines(textLines)
  val mergedSystem = mergeSystems(allSystems)

  println()
  println(mergedSystem)
  println()
  println("Part 1 - Number of beacons: " + mergedSystem.beacons.size)

  val maxManhattan = mergedSystem.scanners
    .flatMap(b1 => mergedSystem.scanners.map(b2 => (b1, b2)))
    .map(_ manhattan _)
    .max

  println("Part 2 - Max scanner Manhattan distance: " + maxManhattan)
}
