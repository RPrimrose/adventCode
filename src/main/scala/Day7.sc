import scala.collection.mutable
import scala.io.Source

case class ParsedLine(name: String, weight: Int, above: List[String])

def parseLine(line: String): ParsedLine = {
  val pattern = "\\w+".r
  val found = pattern.findAllIn(line).toList
  new ParsedLine(found(0), found(1).toInt, found.tail.tail)
}

val filename = "C:\\dev\\advent\\src\\main\\scala\\Day7Input.txt"
val input = Source.fromFile(filename).getLines
val parsedLines = input.map(x => parseLine(x)).toList
var allNodeNames: mutable.MutableList[String] = new mutable.MutableList[String]
for (line <- parsedLines) {
  allNodeNames ++= line.above
}

var rootLine = parsedLines.find(x => x.name == "fbgguv").get

def getWeight(node: ParsedLine, allNodes: List[ParsedLine]): Int = {
  //return weight of this node plus all child nodes
  if (node.above.length != 0) {
    val childNodesWeights = node.above.map(
      x => {
        val node = allNodes.find(y => y.name == x).get
        getWeight(node, allNodes)
      }
    ).sum
    childNodesWeights + node.weight
  }
  else {
    node.weight
  }
}

def findBadOne(node: ParsedLine, allNodes: List[ParsedLine]) {
  val candidates = node.above.map(x => {
    val childLine = parsedLines.find(a => a.name == x).get
    (x, getWeight(childLine, parsedLines))
  })
  candidates.map(x => println(x))
  if (candidates.distinct.length > 1) {
    val badOne = candidates.maxBy(_._2)
    println("Bad one is: " + badOne)
    val badNode = parsedLines.find(a => a.name == badOne._1).get
    findBadOne(badNode, allNodes)
  }
  else {
    println("Bad one was parent")
  }
}

findBadOne(rootLine, parsedLines)
