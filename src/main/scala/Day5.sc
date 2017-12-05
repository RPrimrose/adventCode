import scala.io.Source

case class Instructions(position: Int, jumps: List[Int], stepsSoFar: Int)

def nextValuePart1(currentVal: Int): Int = {
  currentVal + 1
}
def nextValuePart2(currentVal: Int): Int = {
  if (currentVal >= 3) currentVal - 1
  else currentVal + 1
}

def stepsToComplete(instructions: Instructions, nextValue: (Int) => Int): Int = {
  var local = instructions.copy()
  val firstInstruction = local.jumps(local.position)
  var nextPosition = local.position + firstInstruction
  while (nextPosition < local.jumps.length && nextPosition >= 0) {
    val instruction = local.jumps(local.position)
    nextPosition = local.position + instruction
    val nextStepCount = local.stepsSoFar + 1
    val newValue = nextValue(local.jumps(local.position))
    val newJumps = local.jumps.patch(local.position, Seq(newValue), 1)
    local = new Instructions(nextPosition, newJumps, nextStepCount)
  }
  local.stepsSoFar
}

val testInput = List(0, 3, 0, 1, -3)
val filename = "C:\\advent\\adventCode\\src\\main\\scala\\Day5Input.txt"
val input = Source.fromFile(filename).getLines().map(x => x.toInt).toList
val initial = new Instructions(0, testInput, 0)
//val initial = new Instructions(0, input, 0)
stepsToComplete(initial, nextValuePart2)