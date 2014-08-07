object Solution {
	def Try(ah:List[(Long,Long)], k:Int, M:Long):Boolean = {
		val mangoes = ah.map(p => p._1 + (k-1) * p._2).sortWith((x,y) => x < y);
		mangoes.take(k).sum <= M
	}
	def Rec(ah:List[(Long,Long)], l:Int, h:Int, M:Long):Int = {
		if (l >= h) l
		else {
			if (Try(ah, h, M)) h
			else {
				val m = (l + h) / 2
				if (Try(ah, m, M)) {
					Rec(ah, m, h-1, M)
				} else {
					Rec(ah, l, m-1, M)
				}
			}
		}
	}
	def Solve(a:List[Long], h:List[Long], M:Long):Long= {
		val ah = a zip h
		Rec(ah, 0, a.length, M)
	}
	def main(args:Array[String]) {
		val nm = Console.readLine.trim.split(' ').toList.map(x => x.toLong)
		val M = nm.last;
		val a = Console.readLine.trim.split(' ').map(x => x.toLong).toList;
		val h = Console.readLine.trim.split(' ').map(x => x.toLong).toList;
		println(Solve(a, h, M))
	}
}
