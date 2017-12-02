def total(str: String) = {
  val strDigits = str.map(_.asDigit)
  strDigits.zip(strDigits.tail :+ strDigits.head).filter(a => a._1 == a._2).map(a=>a._1).sum
}
val sum = total("91212129")
