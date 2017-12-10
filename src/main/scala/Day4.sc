import scala.io.Source

val filename = "C:\\code\\Advent\\src\\main\\scala\\Day4Input.txt"

def splitRow(row: String) = row.split("\\s+").map(x => x.trim)

def passphraseAllowed(row: String): Int = {
  val split = splitRow(row)
  if (split.distinct.size != split.size) 0
  else 1
}
def passphraseAllowedPart2(row: String): Int = {
  val split = splitRow(row)
  val splitSorted = split.map(x=>x.sorted)
  if (splitSorted.distinct.size != splitSorted.size) 0
  else 1
}

Source.fromFile(filename).getLines.foldLeft(0)((a, b) => a + passphraseAllowed(b))
Source.fromFile(filename).getLines.foldLeft(0)((a, b) => a + passphraseAllowedPart2(b))