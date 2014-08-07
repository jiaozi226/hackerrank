object Solution {
	def GCD(m:Int, n:Int):Int = {
		val a = if (m >= n) m else n
		val b = if (m < n ) m else n
	 	if (a % b == 0)  return  b
		else return GCD(b, a % b)
	}
	def GetPos(r:Int, k:Int, n:Int, m:Int, N:Int, M:Int) :(Int, Int) = {
		if (k >= 0 && k <= (n-1)) {
			(k + r, r)  // left
		} else if (k >= n && k <= (n + m -2)) {
		  (N -1 -r,k - n + r +1)// bottom
		} else if (k >= (n+m-1) && k <= (2*n +m-3)) {
			(N -r -k +m +n -3,M-1-r)// right
		} else {
			(r, M -r -k + 2*n +m -4) // upper
		}
	}
	def GetValue(data:Array[Array[Int]], r:Int, k:Int, n:Int, m:Int, N:Int, M:Int) : Int = {
		val pos = GetPos(r, k, n, m, N,M)
		data(pos._1)(pos._2)
	}
	def SetValue(data:Array[Array[Int]], r:Int, k:Int, n:Int, m:Int, N:Int, M:Int, v:Int) : Unit = {
		val pos = GetPos(r, k, n, m, N, M)
		data(pos._1)(pos._2) = v
	}
	def Solve(N:Int, M:Int, R:Int, data:Array[Array[Int]]):Unit = {
		var r = 0
		var remain = N * M
		// Construct layer info
		var layers:List[(Int, Int, Int)] = List()
		while (remain > 0) {
			val len = 2 * (N - 2*r + M - 2*r) - 4;
			layers = (len, N - 2*r, M - 2*r) :: layers
			remain -= len
			r = r + 1
		}
		layers = layers.reverse
		layers.zipWithIndex.foreach(x => {
		  val r = x._2
			val len = x._1._1
			val n = x._1._2
			val m = x._1._3
			if (R % len != 0) {
			 val d = GCD(len, R)
			 (0 until d).foreach(start => {
					val startPos = start
					var lastPos = startPos
					var nextPos = (startPos + R) % len 
					var lastValue = GetValue(data, r, startPos, n, m, N, M)
					while(nextPos != startPos) {
						val tmp = GetValue(data, r, nextPos, n, m, N, M)
						SetValue(data, r, nextPos, n, m, N,M, lastValue)
						
						lastValue = tmp
						lastPos = nextPos
						nextPos = (nextPos + R) % len
					}
					SetValue(data, r, startPos, n, m, N, M, lastValue)
			 })
			}
		})
	}
	def main(args:Array[String]) {
		val nums = Console.readLine.split(' ').map(_.toInt).toArray
		val N = nums(0)
		val M = nums(1)
		val R = nums(2)
		val data = new Array[Array[Int]](N)
		for (i <- 0 until N) {
			data(i) = Console.readLine.split(' ').map(_.toInt).toArray
		}
		Solve(N, M ,R, data)
		for (i <- 0 until N) {
			println(data(i).mkString(" "))
		}
	}
}

