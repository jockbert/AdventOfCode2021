import scala.annotation.tailrec

case class Board(rows: List[List[Int]]) {

  def hasWinningRow(drawnNumbers: Seq[Int]): Boolean =
    rows.exists(_.forall(drawnNumbers.contains)) ||
      rows.transpose.exists(_.forall(drawnNumbers.contains))

  def score(drawnNumbers: Seq[Int]): Int =
    val lastDrawn = drawnNumbers.head
    val sumOfAllUnmarked = rows.flatten
      .filterNot(drawnNumbers.contains)
      .sum
    lastDrawn * sumOfAllUnmarked
}

@tailrec def findWinningBoard(
    drawn: List[Int],
    notYetDrawn: List[Int],
    boards: List[Board]
): Option[(Board, List[Int])] = {
  val firstWin = boards.find(_.hasWinningRow(drawn))
  (firstWin, notYetDrawn) match {
    case (Some(_), _) => firstWin.map((_, drawn))
    case (_, Nil)     => None
    case _ =>
      findWinningBoard(notYetDrawn.head :: drawn, notYetDrawn.tail, boards)
  }
}

@tailrec def findLoosingBoard(
    drawn: List[Int],
    notYetDrawn: List[Int],
    boards: List[Board]
): Option[(Board, List[Int])] = {
  val winner = findWinningBoard(drawn, notYetDrawn, boards)

  (boards, winner) match {
    // last board left
    case (_ :: Nil, w) => w
    case (_, None)     => None
    case (_, Some(wBoard, wDrawn)) =>
      findLoosingBoard(
        wDrawn,
        notYetDrawn.filterNot(wDrawn.contains),
        boards.filterNot(_.equals(wBoard))
      )
  }
}

def asText(b: Option[(Board, List[Int])]) = b match {
  case Some(winnerBoard, winnerDraw) => winnerBoard.score(winnerDraw)
  case _                             => "no board winner found"
}

@main def main(filePath: String) = {
  println(s"Day 4 (Giant Squid) using file '$filePath'")
  val textLines = scala.io.Source.fromFile(filePath, "UTF-8").getLines.toList
  val allDrawnNumbers = textLines.head.trim.split(",").toList.map(_.toInt)
  val boardLines = textLines.tail.tail.map(_.trim)

  val boards = boardLines
    .filterNot(_.isBlank)
    .map(_.split(" ").map(_.trim).filterNot(_.isEmpty).map(_.toInt).toList)
    .grouped(5)
    .map(Board.apply)
    .toList

  println("part 1: " + asText(findWinningBoard(Nil, allDrawnNumbers, boards)))
  println("part 2: " + asText(findLoosingBoard(Nil, allDrawnNumbers, boards)))
}
