import scala.collection.mutable.HashMap;
object Solution {
	def Solve(N:Int, M:Int, K:Int) : Long = {
		val mod = 1000000007;
		// init
		var lastArr = new Array[HashMap[Int, (Long, Long)]](M);
		var curArr = new Array[HashMap[Int, (Long, Long)]](M);
		for (i <- 0 until M) {
			if (i == 0) {
				lastArr(i) = HashMap(0 ->(1,0))
			} else {
				lastArr(i) = HashMap(0 -> (0, 1));
			}
		}
		for (l <- 1 until N) {
			curArr(0) = HashMap(0 -> (1, 0));
			for (i <- 1 until M) {
				curArr(i) = HashMap(0 -> (0, 0));
				// process element from up
				for ((k,v) <- lastArr(i)) {
					// from up
					if (v._1 != 0) {
						if(curArr(i).contains(k)) {
							val old = curArr(i)(k)
							curArr(i)(k) = ((old._1 + v._1) % mod, old._2 % mod)
						} else {
							curArr(i) += ((k, (v._1 % mod, 0)))
						}
					}
					if (v._2 != 0 && (k+1) <= K) {
						if (curArr(i).contains(k+1)) {
							val old = curArr(i)(k+1)
							curArr(i)(k+1) = ((old._1 + v._2) % mod , old._2 % mod);
						} else {
							curArr(i) += ((k+1, (v._2 % mod, 0)))
						}
					}
				}
				// process element from left
				for ((k,v) <- curArr(i-1)) {
					if (v._1 != 0 && (k+1) <= K) {
						if (curArr(i).contains(k+1)) {
							val old = curArr(i)(k+1)
							curArr(i)(k+1) = (old._1 % mod, (old._2 + v._1) % mod)
						} else {
							curArr(i) += ((k+1,(0, v._1 % mod)))
						}
					}
					if (v._2 != 0) {
						if (curArr(i).contains(k)) {
							val old = curArr(i)(k)
							curArr(i)(k) = (old._1 % mod, (old._2 + v._2) % mod)
						} else {
							curArr(i) += ((k, (0, v._2 % mod)))
						}
					}
				}
				// println(l + " " + i + " " + curArr(i).toString)
			}
			var tmp = lastArr
			lastArr = curArr
			curArr = tmp
		}
		lastArr(M-1).toList.map(x => (x._1, (x._2._1 +  x._2._2) % mod)).reduce((x,y)=>{(x._1, (x._2 + y._2) % mod)})._2
	}
	def main(args:Array[String]) {
		val caseNum = Console.readLine.toInt
		for (i <- 0 until caseNum) {
			val numbers = Console.readLine.split(' ').map(_.toInt)
			val N = numbers(0)
			val M = numbers(1)
			val K = numbers(2)
			println(Solve(N,M,K) % (1000000007))
		}
	}
}
