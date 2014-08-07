object Solution {
	def Solve(n:Int):Int = {
		if (n % 2 != 0) 0
		else {
			val upperBound = math.sqrt(n).toInt;
			var cnt:Int = 0;
			for (k <- 1 to upperBound) {
				if ( n % k == 0) {
					if (k % 2 == 0) cnt = cnt + 1
					if ((n/k != k) && (n/k) % 2 == 0) cnt = cnt + 1
				}
			}
			cnt
		}
	}
	def main(args:Array[String]) {
		val caseNum = Console.readLine.toInt
		for (i <- 0 until caseNum) {
			println(Solve(Console.readLine.toInt))
		}
	}
}
