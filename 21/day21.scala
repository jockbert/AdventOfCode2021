case class Player(pos: Int, score: Int, isPlayer1: Boolean) {
  def act(stepsToMove: Int) =
    val stepsToMoveOnBoard = stepsToMove % 10
    val newPos = pos + stepsToMoveOnBoard
    val newPos2 = newPos + (if (newPos > 10) -10 else 0)
    Player(newPos2, score + newPos2, isPlayer1)
}

trait Die { def rollCount: Int; def values: List[Int]; def roll: Die }

case class DeterministicDie(value: Int = 0, rollCount: Int = 0) extends Die {
  def roll = DeterministicDie(if (value == 100) 1 else value + 1, rollCount + 1)
  def values = value :: Nil
}

case object DiracDie extends Die {
  def values = List(1, 2, 3); def roll = DiracDie
  def rollCount = throw Exception("who's counting?")
}

case class GameState(next: Player, last: Player, die: Die) {
  def looser = if next.score < last.score then next else last
  def winner = if looser == next then last else next
  def isGameWon(scoreToReach: Int): Boolean = winner.score >= scoreToReach
}

/** Universe count per game state map */
type UCountPerState = Map[GameState, Long]

/** State and Universe count tuple */
type StateUCount = (GameState, Long)

def playUntilWin(startState: GameState, scoreToReach: Int): UCountPerState =
  def playLoop(universesPerState: UCountPerState): UCountPerState =
    if (universesPerState.isEmpty) Map.empty
    else {
      val newStates: List[StateUCount] =
        universesPerState.toList.flatMap((state, universeCount) =>
          val die1 = state.die.roll
          val die2 = die1.roll
          val die3 = die2.roll

          die1.values.flatMap(d1 =>
            die2.values.flatMap(d2 =>
              die3.values.map(d3 =>
                val next2 = state.next.act(d1 + d2 + d3)
                (GameState(state.last, next2, die3), universeCount)
              )
            )
          )
        )

      val (won, unfinished) = newStates.partition(_._1.isGameWon(scoreToReach))

      // Goup together similar unfinished game states to reduce data size
      val ongoing = unfinished.groupMapReduce(_._1)(_._2)(_ + _)

      // Group together similar finished game states to reduce data size
      (won ++ playLoop(ongoing).toList).groupMapReduce(_._1)(_._2)(_ + _)
    }
  playLoop(Map(startState -> 1))

@main def main(filePath: String) =
  println(s"Day 21 (Dirac Dice) using file '$filePath'")
  val textLines = scala.io.Source.fromFile(filePath, "UTF-8").getLines.toList
  val startPos1 = textLines.head.drop(28).toInt
  val startPos2 = textLines.tail.head.drop(28).toInt
  val player1 = Player(startPos1, 0, true)
  val player2 = Player(startPos2, 0, false)

  // Part 1 -----------------
  val startPart1 = GameState(player1, player2, DeterministicDie())
  val resultPart1 = playUntilWin(startPart1, 1000).head._1
  val answerPart1 = resultPart1.looser.score * resultPart1.die.rollCount
  println("Part 1, loosing score times dice rolls: " + answerPart1)

  // Part 2 -----------------
  val startPart2 = GameState(player1, player2, DiracDie)
  val allResultsPart2 = playUntilWin(startPart2, 21)
  val winCounts =
    allResultsPart2.toList
      .map((state: GameState, universes: Long) =>
        (if state.winner.isPlayer1 then 1 else 2, universes)
      )
      .groupMapReduce(_._1)(tuple => BigInt(tuple._2))(_ + _)
  println(
    s"Part 2, win count for player 1 is ${winCounts(1)} "
      + s"and win count for player 2 is ${winCounts(2)}"
  )
