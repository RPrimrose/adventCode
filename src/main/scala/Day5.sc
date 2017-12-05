import scala.io.Source

case class Instructions (position: Int, jumps:List[Int], stepsSoFar: Int)

def stepsToComplete(instructions : Instructions) : Int = {
  val instruction = instructions.jumps(instructions.position)
  val nextPosition = instructions.position + instruction
  val nextStepCount = instructions.stepsSoFar + 1
  if (nextPosition >= instructions.jumps.length || nextPosition < 0) nextStepCount
  else {
    val newValue = instructions.jumps(instructions.position) +1
    val newJumps = instructions.jumps.patch(instructions.position, Seq(newValue), 1)
    val newInstructions = new Instructions(nextPosition,newJumps,nextStepCount)
    stepsToComplete(newInstructions)
  }
}

val filename = "C:\\advent\\adventCode\\src\\main\\scala\\Day5Input.txt"
val input = Source.fromFile(filename).getLines().map(x=>x.toInt).toList
val initial = new Instructions(0, input,0)
stepsToComplete(initial)