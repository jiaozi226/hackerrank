
object Solution {
  //
	def RotatedLists(chs:List[Char]):List[List[Char]] = {
		{for (i <- 1 to chs.length) yield chs.drop(i):::chs.take(i)}.toList
	}
	// Main
	def main(args:Array[String]) {
		val caseNum = Console.readLine.trim.toInt
		for (i <- 0 until caseNum) {
			val resList = RotatedLists(Console.readLine.trim.toList)
			println(resList.map(x=>x.mkString("")).mkString(" "))
		}
	}

}
