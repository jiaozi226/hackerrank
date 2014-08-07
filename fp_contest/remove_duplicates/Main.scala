object Solution {
	def Solve(chs:List[Char]) : String = {
		val newChs = chs.zipWithIndex.groupBy( x => x._1).toList.map(x => (x._1, x._2.head._2)).sortWith((x,y) => {x._2 < y._2})
		var sb = new StringBuilder(newChs.length);
		for (x <- newChs) {
			sb = sb + x._1
		}
		return sb.toString
	}
	def main(args: Array[String]) {
		println(Solve(Console.readLine.toList))
	}
}
	
