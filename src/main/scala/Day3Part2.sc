def gridLengthNeededToHoldPort(port: Int): Int = {
  var current = 1
  while (current * current < port) {
    current += 2
  }
  current
}

def positionOf1(sideLength: Int): (Int, Int) = {
  val midpoint = (sideLength + 1) / 2
  (midpoint - 1, midpoint - 1)
}

object Direction extends Enumeration {
  val Right, Up, Left, Down = Value
}

def nextDirection(direction: Direction.Value, a: Array[Array[Int]], pos: (Int, Int)) = {
  direction match {
    case Direction.Right =>
      if (a(pos._1)(pos._2 + 1) == 0)
        Direction.Up
      else Direction.Right
    case Direction.Up =>
      if (a(pos._1 - 1)(pos._2) == 0) Direction.Left
      else Direction.Up
    case Direction.Left =>
      if (a(pos._1)(pos._2 - 1) == 0) Direction.Down
      else Direction.Left
    case Direction.Down =>
      if (a(pos._1 + 1)(pos._2) == 0) Direction.Right
      else Direction.Down
  }
}

def scoreForSquare(a: Array[Array[Int]], pos: (Int, Int)): Int = {
  val b = a(pos._1 - 1)(pos._2 - 1)
  val c =  a(pos._1 - 1)(pos._2)
  val d = a(pos._1 - 1)(pos._2 + 1)
  val e = a(pos._1)(pos._2 - 1)
  val f =  a(pos._1)(pos._2)
  val g = a(pos._1)(pos._2 + 1)
  val h = a(pos._1 + 1)(pos._2 - 1)
  val i = a(pos._1 + 1)(pos._2)
  val j = a(pos._1 + 1)(pos._2 + 1)
  val total = b+c+d+e+f+g+h+i+j
  total

}
def fillArray(length: Int, max: Int): Int = {
  val a = Array.ofDim[Int](length, length)
  var currentDirection = Direction.Right
  var currentValue = 1
  var pos = positionOf1(length)
  a(pos._1)(pos._2) = currentValue
  pos = (pos._1 + 1, pos._2)
  while (currentValue < max) {
    currentValue = scoreForSquare(a,pos)
    a(pos._1)(pos._2) = currentValue
    currentDirection = nextDirection(currentDirection, a, pos)
    currentDirection match {
      case Direction.Right => pos = (pos._1 + 1, pos._2)
      case Direction.Up => pos = (pos._1, pos._2 + 1)
      case Direction.Left => pos = (pos._1 - 1, pos._2)
      case Direction.Down => pos = (pos._1, pos._2 - 1)
    }
  }
  currentValue
}
val port = 347991
val length = gridLengthNeededToHoldPort(port)

fillArray(length, port)
