object Solution {
	def gcd(m:Int, n:Int) :Int = {
		val max_one = if (m >= n) m else n
		val min_one = if (m < n) m else n
		if (min_one <  1) max_one
		else gcd (max_one % min_one, min_one)
	}
	def Solve(a:Int, b:Int, c:Int):String = {
	  if (c > (if (a > b) a else b)) "NO"
		else {
			val g = gcd(a,b)
			if (c % g == 0) "YES"
			else "NO"
		}
	}
	def main(args:Array[String]) {
		val caseNum = Console.readLine.toInt
		for (i <- 0 until caseNum) {
			val values = Console.readLine.split(' ').map(_.toInt)
			println(Solve(values(0), values(1), values(2)))
		}
	}
}
