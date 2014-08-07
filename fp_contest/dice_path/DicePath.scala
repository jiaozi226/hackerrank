class Dice(val top:Int, val front:Int, val left:Int) {
	def rotateRight:Dice = {
		new Dice(this.left, this.front, 7 - this.top)
	}
	def rotateDown:Dice = {
		new Dice(7 - this.front, this.top, this.left)
	}
}
class State(val dices:List[Dice],var result:Int) {
	def rotateRight:State = {
		val candidates = dices.map(x => {
			val y = x.rotateRight
			new State(List(y), y.top + result);
		})
		candidates.reduce((x,y) => if (x.result < y.result) y else x)
	}
	def rotateDown:State = {
		val candidates = dices.map(x => {
			val y = x.rotateDown
			new State(List(y), y.top + result)
		})
		candidates.reduce((x,y) => if (x.result < y.result) y else x)
	}
}

object Solution {
	def Compute(states:Array[Array[State]], i:Int, j:Int):State ={
		if (states(i)(j) != null) states(i)(j)
		else {
			val up = Compute(states, i-1, j)
			val left = Compute(states, i, j-1);
			val fromUp = up.rotateDown
			val fromLeft = left.rotateRight
			if (fromUp.result > fromLeft.result) {
				states(i)(j) = fromUp
			} else if (fromUp.result < fromLeft.result) {
				states(i)(j) = fromLeft
			} else {
				states(i)(j) = new State(List(fromUp.dices.head, fromLeft.dices.head), fromUp.result)
			}
			states(i)(j)
		}
	}
  def Solve(m:Int, n:Int) = {
		val states = new Array[Array[State]](m + 1);
		for (i <- 0 to m) {
			states(i) = new Array[State](n + 1)
			for (j <- 0 to n) {
				states(i)(j) = null
			}
		}
		states(1)(1) = new State(List(new Dice(1,2,3)),1);
		for (j <- 2 to n) {
			states(1)(j) = states(1)(j-1).rotateRight
		}
		for (i <- 2 to m) {
			states(i)(1) = states(i-1)(1).rotateDown
		}
		Compute(states, m, n).result
	}
	def main(args:Array[String]) {
		val caseNum = Console.readLine.trim.toInt
		for (k <- 1 to caseNum) {
			val arr = Console.readLine.trim.split(' ').map(x => x.toInt)
			val m = arr(0)
			val n = arr(1)
			println(Solve(m,n))
		}
	}
}
