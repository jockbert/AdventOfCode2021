sealed trait Packet { def calc: BigInt; def versions: BigInt; def show: String }

case class Literal(versions: BigInt, calc: BigInt) extends Packet {
  def show = calc.toString
}

case class Operator(ver: Int, pks: List[Packet], op: Int) extends Packet {
  def calc = {
    val values = pks.map(_.calc)
    op match
      case 0 => values.sum
      case 1 => values.product
      case 2 => values.min
      case 3 => values.max
      case 5 => if values(0) > values(1) then 1 else 0
      case 6 => if values(0) < values(1) then 1 else 0
      case 7 => if values(0) == values(1) then 1 else 0
  }
  def versions = ver + pks.map(_.versions).sum
  def show = "(" + (op match
    case 0 => "+"
    case 1 => "*"
    case 2 => "min"
    case 3 => "max"
    case 5 => ">"
    case 6 => "<"
    case 7 => "="
  ) + " " + pks.map(_.show).mkString(" ") + ")"
}

case class BitMark(mark: Int, br: BitReader) { def distance = br.pos - mark }

case class BitReader(bits: IndexedSeq[Boolean]) {
  var pos = 0
  def mark = BitMark(pos, this)
  def read(bitCount: Int) = {
    def readBit = { val b = bits(pos); pos += 1; b }
    if (bits.size - bitCount < pos) throw RuntimeException(s"parsing past end")
    else
      (1 to bitCount)
        .map(_ => readBit)
        .foldLeft(0)((res, bit) => (res << 1) + (if bit then 1 else 0))
  }
}

case class Parser(bp: BitReader) {

  private def parseLiteral(res: BigInt = 0): BigInt =
    if bp.read(1) == 0 then (res << 4) + BigInt(bp.read(4))
    else parseLiteral((res << 4) + bp.read(4))

  private def parseSubPksTotLen(totLen: Int, start: BitMark): List[Packet] =
    if (start.distance >= totLen) Nil
    else parsePacket() :: parseSubPksTotLen(totLen, start)

  private def parseSubPksCounting(count: Int): List[Packet] =
    if (count <= 0) Nil else parsePacket() :: parseSubPksCounting(count - 1)

  private def parseSubPackets(): List[Packet] = bp.read(1) match
    case 0 => parseSubPksTotLen(bp.read(15), bp.mark)
    case 1 => parseSubPksCounting(bp.read(11))

  def parsePacket() = {
    val version = bp.read(3)
    val typeId = bp.read(3)
    typeId match
      case 4 => Literal(version, parseLiteral())
      case x => Operator(version, parseSubPackets(), x)
  }
}

def toNibbleBits(n: Int) = (3 to 0 by -1).map(bit => ((n >> bit) & 1) == 1)

def parseAndShowHex(hex: String) = {
  println()
  println("hex         = " + hex)
  val bits = hex.map(_.asDigit).flatMap(toNibbleBits)
  val parser = Parser(BitReader(bits))
  val packet = parser.parsePacket()
  println("version sum = " + packet.versions)
  println("expression  = " + packet.show)
  println("            = " + packet.calc)
}

@main def main(filePath: String) =
  println(s"Day 16 (Packet Decoder) using file '$filePath'")
  scala.io.Source.fromFile(filePath, "UTF-8").getLines.foreach(parseAndShowHex)
