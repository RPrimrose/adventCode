import scala.io.Source

def countGroups(input: String): Int = {
  var groupLevel:Int = 0
  var escaped = false
  var inGarbage = false
  var garbageCount = 0
  var score:Int = 0
  input.map(character => {
    character match {
      case '!' => {
        escaped = !escaped
      }
      case '{' => {
        if (!inGarbage) {
          groupLevel += 1
          score += groupLevel
        }
        else if (inGarbage && !escaped){
          garbageCount+=1
        }
        escaped = false
      }
      case '}' => {
        if (!inGarbage) groupLevel -= 1
        else if (inGarbage && !escaped){
          garbageCount+=1
        }
        escaped = false
      }
      case '<' => {
        if (inGarbage && !escaped) garbageCount+=1
        inGarbage = true
        escaped = false
      }
      case '>' => {
        if (!escaped) inGarbage = false
        escaped = false
      }
      case _ => {
        if (inGarbage && !escaped) garbageCount+=1
        if (escaped) {escaped = false}
      }
    }
  })
  println("garbagecount =" + garbageCount)
  score
}

//countGroups("{}")
//countGroups("{{{}}}")
//countGroups("{{},{}}")
//countGroups("{{{},{},{{}}}}")
//countGroups("{<a>,<a>,<a>,<a>}")
//countGroups("{{<ab>},{<ab>},{<ab>},{<ab>}}")
//countGroups("{{<!!>},{<!!>},{<!!>},{<!!>}}")
//countGroups("{{<a!>},{<a!>},{<a!>},{<ab>}}")
val filename = "C:\\dev\\advent\\src\\main\\scala\\Day9Input.txt"
val input = Source.fromFile(filename).getLines().mkString
countGroups(input)
countGroups("<random characters>")
countGroups("<<<<>")
countGroups("<{!>}>")
countGroups("<!!>")
countGroups("<!!!>>")
countGroups("<{o\"i!a,<{i<a>")
