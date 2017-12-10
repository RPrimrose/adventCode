import scala.collection.mutable
import scala.io.Source

def indexOfBlockToUse(blocks:Array[Int]): Int = {
  val max = blocks.max
  blocks.indexOf(max)
}

def updateList(blocks:List[Int]): List[Int] = {
  val local = blocks.toArray
  val index = indexOfBlockToUse(local)
  var numToDistribute = local(index)
  local(index) = 0
  var position = (index+1)%local.length
  while (numToDistribute > 0){
    local(position)+=1
    numToDistribute-=1
    position = (position+1)%local.length  }
  local.toList
}

def part1(blocks:List[Int]):Int = {
  var lists = mutable.MutableList[List[Int]](blocks)
  while (lists.distinct.length == lists.length){
    val currentBlock = lists.takeRight(1).head
    var newList = updateList(currentBlock)
//    println("newlist is " + newList.mkString(","))
    lists = lists += newList
  }
  lists.length-1
}

def part2(blocks:List[Int]):Int = {
  var lists = mutable.MutableList[List[Int]](blocks)
  while (lists.distinct.length == lists.length){
    val currentBlock = lists.takeRight(1).head
    var newList = updateList(currentBlock)
    //    println("newlist is " + newList.mkString(","))
    lists = lists += newList

  }
  println(lists.indexOf(lists.takeRight(1).head))
  lists.length-1
}
def splitRow(row: String): List[Int] = row.split("\\s+").map(x => x.trim.toInt).toList

val firstInput = List(0,2,7,0)
val filename = "C:\\dev\\advent\\src\\main\\scala\\Day6Input.txt"
val input = Source.fromFile(filename).getLines.mkString
val split = splitRow(input)
part2(split)
