import scala.annotation.tailrec

/* Max length reached given initial velocity and retardation 1 per step. */
def maxLength(vel: Int) = vel * (vel + 1) / 2

def searchMinVelToReachLength(pos: Int, vel: Int = 0): Int =
  if (maxLength(vel) >= pos) vel else searchMinVelToReachLength(pos, vel + 1)

case class Probe(initXVel: Int, initYVel: Int) {
  def maxYPos = if (initYVel < 0) 0 else maxLength(initYVel)

  def doesItReachTarget(xTarget: Range, yTarget: Range): Boolean = {
    def slowDown(n: Int) = if n > 0 then n - 1 else if n < 0 then n + 1 else 0

    @tailrec def loop(xVel: Int, yVel: Int, xPos: Int, yPos: Int): Boolean =
      if (yPos < yTarget.start) false
      else if (yTarget.contains(yPos) && xTarget.contains(xPos)) true
      else loop(slowDown(xVel), yVel - 1, xPos + xVel, yPos + yVel)

    loop(initXVel, initYVel, 0, 0)
  }
}

def span(s: String) = { val a = s.split("\\.\\.").map(_.toInt); (a(0) to a(1)) }

@main def main(filePath: String) = {
  println(s"Day 17 (Trick Shot) using file '$filePath'")
  val line = scala.io.Source.fromFile(filePath, "UTF-8").getLines.next
  val xTarget = span(line.split("=")(1).split(",")(0))
  val yTarget = span(line.split("=")(2))

  // Because of symetry of gravity on the way up and down, y-position will
  // always pass y=0 on the way down with initial y-velocity negated.
  // Reverse y-range, so initial y velocity with highest y-max comes first.
  val xVels = (searchMinVelToReachLength(xTarget.start) to xTarget.end)
  val yVels = (yTarget.start to -yTarget.start).reverse

  val candidates = yVels
    .flatMap(y => xVels.map(x => Probe(x, y)))
    .filter(_.doesItReachTarget(xTarget, yTarget))

  val maxYProbe = candidates.head
  val distinctVelocities = candidates.size

  println("target x            : " + xTarget)
  println("target y            : " + yTarget)
  println("candidate y init vel: " + yVels)
  println("candidate x init vel: " + xVels)
  println("max Y probe winner  : " + maxYProbe)
  println("max Y               : " + maxYProbe.maxYPos)
  println("distinct velocities : " + distinctVelocities)
}
