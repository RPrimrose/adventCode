import scala.io.Source


case class Inst(name: String, change: String, changeAmount: Int, condition: String)

def parseLine(line: String): Inst = {
  val pattern = "(\\w+)\\s(inc|dec)\\s(-?\\d+)\\sif\\s(.+)".r
  val found = pattern.findAllIn(line)
  new Inst(found.group(1), found.group(2), found.group(3).toInt, found.group(4))
}
def checkCondition(condition: String, registers: scala.collection.mutable.Map[String, Int]): Boolean = {
  val pattern = "(\\w+)\\s(.+)\\s(-?\\w+)".r
  val found = pattern.findAllIn(condition)
  val first = found.group(1)
  if (!registers.contains(first)) {
    registers += (first-> 0)
  }
  val second = found.group(3).toInt
  found.group(2) match {
    case "<" => registers(first) < second
    case "<=" => registers(first) <= second
    case "==" => registers(first) == second
    case ">" => registers(first) > second
    case ">=" => registers(first) >= second
    case "!=" => registers(first) != second
    case _ => throw new RuntimeException("You forgot one" + found.group(2))
  }
}

def processLine(instruction: Inst, registers: scala.collection.mutable.Map[String, Int]): Unit = {
  if (!registers.contains(instruction.name)) {
    registers += (instruction.name -> 0)
  }
  if (checkCondition(instruction.condition, registers)) {
    instruction.change match {
      case "inc" => registers(instruction.name) = registers(instruction.name) + instruction.changeAmount
      case "dec" => registers(instruction.name) = registers(instruction.name) - instruction.changeAmount
      case _ => throw new RuntimeException("something went wrong")
    }
  }
}

val filename = "C:\\dev\\advent\\src\\main\\scala\\Day8Input.txt"
val input = Source.fromFile(filename).getLines()
val registers: scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map()
var maxEver = 9
for (line<- input){
  val parsed = parseLine(line)
  processLine(parsed, registers)
  if (registers.maxBy(_._2)._2 > maxEver){
    maxEver = registers.maxBy(_._2)._2
  }
}
registers.maxBy(_._2)
maxEver