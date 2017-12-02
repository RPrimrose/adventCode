def splitRow(row: String) = row.split("\\s+").map(x => x.trim.toInt)

def rowDigit(row: String): Int = {
  val split = splitRow(row)
  split.max - split.min
}

def evenlyDividesVec(a: Vector[Int]): Int = {
  val result = a.max.toDouble / a.min.toDouble
  if (result % 1 == 0) {
    result.toInt
  }
  else 0
}
def rowDivides(row: String): Int = {
  val split = splitRow(row)
  val subSets = split.toSet.subsets(2)
  subSets.foldLeft(0)((a, b) => a + evenlyDividesVec(b.toVector))
}

import scala.io.Source

val filename = "C:\\dev\\advent\\src\\main\\scala\\Day2Input.txt"
Source.fromFile(filename).getLines.foldLeft(0)((a, b) => a + rowDigit(b))
Source.fromFile(filename).getLines.foldLeft(0)((a, b) => a + rowDivides(b))
