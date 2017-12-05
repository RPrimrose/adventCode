def gridLengthNeededToHoldPort(port: Int): Int = {
  var current = 1
  while (current*current < port){
    current+=2
  }
  current
}


def positionOf1(sideLength:Int):(Int,Int) = {
  val midpoint = (sideLength+1)/2
  (midpoint,midpoint)
}

def taxicab(p1:Int, p2:Int, q1:Int, q2:Int):Int ={
  Math.abs(p1-q1)+Math.abs(p2-q2)
}

def distToCorner(port:Int, length:Int):Int = {
  val corner = length*length
  val maxDist = length-1
  var temp = port
  while(temp+maxDist<=(corner+(maxDist/2))){
    temp+=maxDist
  }
  Math.abs(corner - temp)
}

val port = 347991
val length = gridLengthNeededToHoldPort(port)
val stepsSaved = distToCorner(port,length)
val pos = positionOf1(length)
taxicab(pos._1, pos._2, length, length)-stepsSaved

