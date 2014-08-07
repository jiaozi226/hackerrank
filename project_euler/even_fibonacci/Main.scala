object Solution {
	def Solve(N:BigInt):BigInt = {
		lazy val fibs = {
			def f(a:BigInt, b:BigInt):Stream[BigInt] = a #:: f(b, a+b)
			f(0,1)
		}
		fibs.filter(x => x % 2 == 0).takeWhile(x => x <= N).sum
	}
	def main(args:Array[String]) {
		val caseNum = Console.readLine.toInt
		for (i <- 1 to caseNum) {
			val N = BigInt(Console.readLine)
			println(Solve(N))
		}
	}
}
