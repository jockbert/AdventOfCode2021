import scala.annotation.tailrec

enum Reg:
  case W, X, Y, Z

import Reg._

case class ALU(w: Int = 0, x: Int = 0, y: Int = 0, z: Int = 0) {

  def apply(r: Reg): Int = r match {
    case W => w
    case X => x
    case Y => y
    case Z => z
  }

  def apply(r: Reg, newValue: Int) = r match {
    case W => copy(w = newValue)
    case X => copy(x = newValue)
    case Y => copy(y = newValue)
    case Z => copy(z = newValue)
  }
}

trait RegRelated {
  def regs: List[Reg]
}

sealed trait Value extends RegRelated {
  def apply(x: ALU): Int
}

case class Literal(n: Int) extends Value {
  def apply(x: ALU) = n
  def regs = Nil
}

case class Variable(r: Reg) extends Value {
  def apply(x: ALU) = x(r)
  def regs = r :: Nil
}

sealed trait Instr extends RegRelated
type Prog = List[Instr]

case class Inp(a: Reg) extends Instr {
  def regs = a :: Nil
}
case class Add(a: Reg, b: Value) extends Instr {
  def regs = a :: b.regs
}
case class Mul(a: Reg, b: Value) extends Instr {
  def regs = a :: b.regs
}
case class Div(a: Reg, b: Value) extends Instr {
  def regs = a :: b.regs
}
case class Mod(a: Reg, b: Value) extends Instr {
  def regs = a :: b.regs
}
case class Eql(a: Reg, b: Value) extends Instr {
  def regs = a :: b.regs
}

case class Nodes(var data: Map[ALU, Long] = Map.empty) {
  def alus = data.keys
  def input(alu: ALU): Long = data(alu)

  /** Add ALU and input (or update input if larger) */
  def addBestInput(alu: ALU, input: Long, keepMax: Boolean): Nodes =
    data = data.updatedWith(alu) {
      case Some(oldInput) if keepMax && oldInput > input  => Some(oldInput)
      case Some(oldInput) if !keepMax && oldInput < input => Some(oldInput)
      case _                                              => Some(input)
    }; this
}

@tailrec def breadthFirstSearch(
    prog: Prog,
    keepMaxInput: Boolean,
    nodes: Nodes = Nodes(Map(ALU() -> 0L))
): Nodes = {

  def execInstrOnNodes(a: Reg, b: Value, fn: (Int, Int) => Int) =
    nodes.alus.foldLeft(Nodes())((ns, alu) =>
      ns.addBestInput(
        alu(a, fn(alu(a), b(alu))),
        nodes.input(alu),
        keepMaxInput
      )
    )

  println("Node count " + nodes.data.size)
  println()
  println("" + prog.headOption + " [" + prog.size + "]")

  if prog.isEmpty then nodes
  else
    val x = prog.head match {
      case Add(a, b) => execInstrOnNodes(a, b, _ + _)
      case Mul(a, b) => execInstrOnNodes(a, b, _ * _)
      case Div(a, b) => execInstrOnNodes(a, b, _ / _)
      case Mod(a, b) => execInstrOnNodes(a, b, _ % _)
      case Eql(a, b) =>
        execInstrOnNodes(a, b, (n1, n2) => if n1 == n2 then 1 else 0)
      case Inp(a) =>
        nodes.alus.foldLeft(Nodes())((ns, alu) =>
          (1 to 9).foreach(n =>
            ns.addBestInput(alu(a, n), nodes.input(alu) * 10 + n, keepMaxInput)
          )
          ns
        )
    }

    breadthFirstSearch(prog.tail, keepMaxInput, x)
}

def parseReg(r: String): Reg = r.trim match {
  case "w" => W; case "x" => X; case "y" => Y; case "z" => Z
  case _   => throw Exception(s"Unknown register $r")
}

def parseValue(v: String): Value = v.trim.toIntOption match {
  case Some(n) => Literal(n); case None => Variable(parseReg(v))
}

def parseInstr(ins: String): Instr = ins.split(" ").toList match {
  case "inp" :: a :: Nil      => Inp(parseReg(a))
  case "add" :: a :: b :: Nil => Add(parseReg(a), parseValue(b))
  case "mul" :: a :: b :: Nil => Mul(parseReg(a), parseValue(b))
  case "div" :: a :: b :: Nil => Div(parseReg(a), parseValue(b))
  case "mod" :: a :: b :: Nil => Mod(parseReg(a), parseValue(b))
  case "eql" :: a :: b :: Nil => Eql(parseReg(a), parseValue(b))
  case x                      => throw Exception(s"Unknown instruction '$ins'")
}

@tailrec def reorderOpt(fwd: Prog, back: Prog = Nil): Prog = {
  fwd match {

    // Return when fwd is consumed - reverse backward result
    case Nil => back.reverse.toList

    // Do not reorder competing resetting to zero
    case (i1 @ Mul(a, Literal(0))) :: (i2 @ Mul(b, Literal(0))) :: rest =>
      reorderOpt(rest, i2 :: i1 :: back)

    // Advance resetting reg to zero - decreases number of possible ALU states
    case i1 :: (i2 @ Mul(a, Literal(0))) :: rest if !i1.regs.contains(a) =>
      reorderOpt(rest, i1 :: i2 :: back)

    // Do not reorder competing reading input
    case (i1 @ Inp(a)) :: (i2 @ Inp(b)) :: rest =>
      reorderOpt(i2 :: rest, i1 :: back)

    // Postpone reading input - increases number of possible ALU states
    case (i1 @ Inp(a)) :: i2 :: rest if !i2.regs.contains(a) =>
      reorderOpt(rest, i1 :: i2 :: back)

    // In all other cases do nothing
    case i :: rest => reorderOpt(rest, i :: back)
  }
}

@tailrec def reduceSearchStatesInProgram(p: Prog): Prog =
  val q = reorderOpt(p)
  println(".")
  q.foreach(println)
  if p == q then q else reduceSearchStatesInProgram(q)

@tailrec def containsZero(n: Long): Boolean =
  if n == 0 then false else if n % 10 == 0 then true else containsZero(n / 10)

@tailrec def toInput(n: Long, res: List[Int] = Nil): List[Int] =
  if n == 0 then res.reverse
  else toInput(n / 10, (n % 10).toInt :: res)

def findModelNumbers(p: Prog, keepMaxInput: Boolean): List[Long] =
  breadthFirstSearch(p, keepMaxInput).data.toList
    .filter { case (alu, in) => alu.z == 0 }
    .map(_._2)

@main def main(filePath: String) = {
  println(s"Day 24 (Arithmetic Logic Unit) using file '$filePath'")
  println("WARNING! This script requires at least 12 GB of heap space")
  println("and goes faster with more of it. Set JVM option \"-Xmx12G\"")
  val textLines = scala.io.Source.fromFile(filePath, "UTF-8").getLines.toList
  val program = textLines.map(parseInstr)

  println("\nReducing space requirements with peephole opt. of ALU instr...")
  val p2 = reduceSearchStatesInProgram(program)

  println("\nCalculating part 1...")
  val part1 = findModelNumbers(p2, true).max
  println("\nCalculating part 2...")
  val part2 = findModelNumbers(p2, false).min

  println("\nPart 1: " + part1)
  println("Part 2: " + part2)
}
