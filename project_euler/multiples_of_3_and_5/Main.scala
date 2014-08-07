object Solution {
	def sum(n:Int, x:Int):Long = {
		val num:Long= (n-1) / x;
		return (num + 1) * num * x / 2;
	}
	def solve(n:Int):Long = {
		sum(n, 3) + sum(n, 5) - sum(n,15)
	}
	def main(args:Array[String]) {
		val caseNum = Console.readLine.toInt
		for (i <- 1 to caseNum) {
			val n = Console.readLine.toInt
			println(solve(n))
		}
	}
}
