class Block(var maxValue:Int, var range:(Int, Int));
object Solution {
  def MergeBlock(left:Block, right:Block, h:Array[Int]) :Block = {
		var p:Int = left.range._2;
		var q:Int = right.range._1;
		var curHeight = if (h(left.range._2) < h(right.range._1)) h(left.range._2) else h(right.range._1)
		while (p >= left.range._1 && h(p) >= curHeight) p = p -1
		while (q <= right.range._2 && h(q) >= curHeight) q = q+1
		var curArea = (left.range._2 - p + q - right.range._1) * curHeight;
		var maxArea = curArea

		while(p >= left.range._1 ||  q <= right.range._2) {
			if (q > right.range._2 || (p >=left.range._1 && h(p) >= h(q))) {
				curHeight = h(p)
				while(p >= left.range._1 && h(p) >= curHeight) p = p -1
			} else {
				curHeight = h(q)
				while(q <= right.range._2 && h(q) >= curHeight) q = q + 1
			}
			curArea = (left.range._2 - p + q - right.range._1) * curHeight 
			if (curArea > maxArea) maxArea = curArea
		}
		var max_value = if (left.maxValue > right.maxValue) left.maxValue else right.maxValue
		max_value = if(max_value > maxArea) max_value else maxArea
		new Block(max_value, (left.range._1, right.range._2))
	}
	def Solve(n:Int, h:Array[Int]):Int = {
		var blockArr = h.zipWithIndex.map(x => {new Block(x._1, (x._2, x._2))});
		while (blockArr.length > 1) {
		  val size = blockArr.length;
			val tmpArr = new Array[Block](size / 2 + (if (size % 2 == 0) 0 else 1));
			for (i <- 0 until size / 2) {
				val idx1 = 2 * i
				val idx2 = idx1 + 1
				tmpArr(i) = MergeBlock(blockArr(idx1), blockArr(idx2), h)
			}
			if (size % 2 != 0) tmpArr(size / 2) = blockArr(size - 1)
			blockArr = tmpArr
		}
		blockArr(0).maxValue
	}
	def main(args:Array[String]) {
		val n = Console.readLine.toInt
		val h = Console.readLine.split(' ').toArray.map(_.toInt)
		println(Solve(n,h));
	}
}
