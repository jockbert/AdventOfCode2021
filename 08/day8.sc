type Pattern = String

case class Entry(keys: List[Pattern], outputs: List[Pattern]) {

  /** Filter out keys with given number of segments. */
  def keysWithSegmentCount(count: Int): List[Pattern] =
    keys.filter(_.size == count)

  /** Predicate to find keyA with given segments diff compared with keyB. */
  def segmentDiff(keyB: Pattern, expectedDiff: Int)(keyA: Pattern): Boolean =
    keyA.diff(keyB).size == expectedDiff

  /** Deduces map between 7-segment code (pattern keys) and digit. */
  lazy val digitPerKey: Map[Pattern, Int] = {

    val key1: Pattern = keysWithSegmentCount(2).head
    val key7: Pattern = keysWithSegmentCount(3).head
    val key4: Pattern = keysWithSegmentCount(4).head
    val key8: Pattern = keysWithSegmentCount(7).head

    val keys235: List[Pattern] = keysWithSegmentCount(5)
    val key2: Pattern = keys235.filter(segmentDiff(key4, 3)).head
    val key3: Pattern = keys235.filter(segmentDiff(key1, 3)).head
    val key5: Pattern = keys235.diff(key2 :: key3 :: Nil).head

    val keys069: List[Pattern] = keysWithSegmentCount(6)
    val key6: Pattern = keys069.filter(segmentDiff(key1, 5)).head
    val key9: Pattern = keys069.filter(segmentDiff(key4, 2)).head
    val key0: Pattern = keys069.diff(key6 :: key9 :: Nil).head

    ((key0 -> 0) :: (key1 -> 1) :: (key2 -> 2) :: (key3 -> 3) :: (key4 -> 4) ::
      (key5 -> 5) :: (key6 -> 6) :: (key7 -> 7) :: (key8 -> 8) :: (key9 -> 9) ::
      Nil).toMap
  }

  /** Calculate number represented by output patterns. */
  def calcOutputNumber() =
    outputs.foldLeft(0)((acc: Int, out: Pattern) => acc * 10 + digitPerKey(out))

  /** Sort segments in all patterns (characters in strings). */
  def normalizePatterns(): Entry =
    Entry(keys.map(_.sorted), outputs.map(_.sorted))
}

@main def main(filePath: String) = {
  println(s"Day 8 (Seven Segment Search) using file '$filePath'")
  val textLines = scala.io.Source.fromFile(filePath, "UTF-8").getLines.toList

  val allEntries: List[Entry] = textLines
    .map(_.split(" \\| "))
    .map(arr => Entry(arr(0).split(" ").toList, arr(1).split(" ").toList))
    .map(_.normalizePatterns())

  val allLengths = allEntries.flatMap(_.outputs.map(_.size))
  val ones = allLengths.count(_ == 2)
  val sevens = allLengths.count(_ == 3)
  val fours = allLengths.count(_ == 4)
  val eights = allLengths.count(_ == 7)
  println("part 1: " + (ones + sevens + fours + eights))
  println("part 2: " + allEntries.map(_.calcOutputNumber()).sum)
}
