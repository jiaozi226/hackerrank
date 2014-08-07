object Solution {
  def Solve(s:Int) : BigDecimal= {
		println(s)
		if (s == 1) 1.0
		else {
		  var stop = false
		  var k = s
		  var arr = new Array[BigDecimal](s-1)
		  var res:BigDecimal= 0.0
			var j = s - 1
			var constProb:BigDecimal = 1.0
			(1 to s).foreach(i => {constProb = constProb * i / s})
		  arr.zipWithIndex.foreach(x=>{
		  	arr(x._2) = 0.0
		  })
			arr(0) = s
			var lastProb:BigDecimal= 0.0
			var thisProb:BigDecimal= 0.0
		  while((k-s) < 5000) {
		  // while((!stop)) {
		    val m = k - s
		  	arr(0) = arr(0) * 1.toDouble/s.toDouble
		  	for (i <- 1 until arr.length) {
		  		arr(i) = (arr(i) * (i+1).toDouble / s.toDouble + arr(i-1))
		  	}
		  	thisProb = (k * arr(arr.length - 1)) * constProb
		  	res += thisProb 
				println("round " + m + " " + arr(arr.length - 1) + " " + j + " " + thisProb + " " + res)
				// val abs = if(lastProb > thisProb)  (lastProb - thisProb)  else (thisProb - lastProb)
				// if ((lastProb - thisProb) > 0 && (lastProb - thisProb) < 0.001) stop = true
		  	k = k + 1
		  }
			println((k-s) + " rounds")
		  res
		}
	}
	def main(args:Array[String]) = {
		val numbers = Console.readLine.split(' ').map(_.toInt)
		println(Solve(numbers(0) * numbers(1)))
	}
}
