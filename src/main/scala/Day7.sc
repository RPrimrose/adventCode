import scala.collection.mutable
import scala.io.Source

case class Node(name:String,  weight:Int, above:List[String])

def parseLine(line:String): Node = {
  val pattern = "\\w+".r
  val found = pattern.findAllIn(line).toList
  new Node(found(0), found(1).toInt, found.tail.tail)
}
val sample = "txszqu (687) -> mvjqmad, lwqlyjq, jlgnsu"
parseLine(sample)
val filename = "C:\\dev\\advent\\src\\main\\scala\\Day7Input.txt"
val input = Source.fromFile(filename).getLines
val nodes = input.map(x=>parseLine(x)).toList
var allNodes: mutable.MutableList[String] = new mutable.MutableList[String]
for (node <- nodes){
  allNodes ++= node.above
}


println(nodes.filter(x => !allNodes.contains(x.name)))
