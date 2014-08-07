
class JumpTreeNode (var branches:List[(Int, JumpTreeNode)], var steps:List[Int])

// RunningMedianTreeNode
class RMTNode(var st:Int, var ed:Int, var cnt:Int)
class RunningMedianTree(val eleArr:Array[Int]) {
	// build tree
	val tree:Array[Array[RMTNode]] = {
		var sizeList:List[Int] = List()
		var size = eleArr.length
		while(size > 0) {
			sizeList = size::sizeList
			val newSize = (size / 2 + (if (size == 1) 0 else if (size % 2 == 0) 0 else 1))
			size = newSize
		}
		// println(sizeList)
		val t = new Array[Array[RMTNode]](sizeList.length)
		t(sizeList.length - 1) = eleArr.map(x=>{new RMTNode(x,x,0)}).toArray
		for (level <- sizeList.length - 2 to 0 by -1) {
			t(level) = new Array[RMTNode](sizeList(level))
			for (i <- 0 until t(level).length) {
				val lidx = i * 2
				val ridx =  lidx + 1
				if (ridx < t(level+1).length) {
					t(level)(i) = new RMTNode(t(level+1)(lidx).st, t(level+1)(ridx).ed, t(level+1)(lidx).cnt + t(level+1)(ridx).cnt)
				} else {
					t(level)(i) = new RMTNode(t(level+1)(lidx).st, t(level+1)(lidx).ed, t(level+1)(lidx).cnt)
				}
			}
		}
		t
	}
	def Update(v:Int, insert:Boolean):Unit = {
		var level = 0
		var idx = 0
		while(level < tree.length) {
		   if (v >= tree(level)(idx).st && v<= tree(level)(idx).ed) {
			 	 idx = idx
			 } else {
			 	 idx = idx + 1
			 }

			// Update count
			if (insert)
				tree(level)(idx).cnt = tree(level)(idx).cnt + 1
			else
				tree(level)(idx).cnt = tree(level)(idx).cnt - 1
			// Update index
			idx = 2*idx
			// Update level
			level = level + 1
		}
	}
	def QueryMedian():Int = {
		var idx = 0
		var level = 0
		var need = tree(level)(idx).cnt / 2  + (if (tree(level)(idx).cnt % 2 == 0) -1 else 0)
		while((level + 1) < tree.length) {
			val lidx = 2* idx
			val ridx = lidx + 1
			if (tree(level+1)(lidx).cnt > need) {
				idx = lidx
			} else {
				idx = ridx
				need = need - tree(level+1)(lidx).cnt
			}
			level = level + 1
		}
		tree(tree.length-1)(idx).st
	}
}

object Solution {
	def Solve(commands:Array[Int]):Array[Int] = {
		/////////// step1:Build the Jump tree /////////////
		val nodeArr = new Array[JumpTreeNode](commands.length + 1)
		val result = new Array[Int](commands.length+1)
		for (i <- 0 until nodeArr.length) nodeArr(i) = null
		nodeArr(0) = new JumpTreeNode(List(), List(0))
		var curNode = nodeArr(0) 
		commands.zipWithIndex.foreach(x => {
			val step = x._2 + 1
			val command = x._1
			if (command > 0) {
				val newNode = new JumpTreeNode(List(), List(step))
				curNode.branches = (command, newNode)::curNode.branches
				curNode = newNode
			} else {
				val bk_idx = step + command
				curNode = nodeArr(bk_idx)
				curNode.steps = step::curNode.steps
			}
			nodeArr(step) = curNode
		})
		// nodeArr.foreach(x => println(x.steps))

		////////// step 2: Build the running median query tree/////
		val eleArr = commands.filter(x=> x > 0).groupBy(x=>x).toArray.map(x=>x._1).sortWith((x,y)=>x<y)
    val tree = new RunningMedianTree(eleArr)
    ////////// step 3: Do the dfs for jump tree //////////////
		var stack = List((0, nodeArr(0)))
		while(stack.nonEmpty) {
			val head = stack.head
			stack = stack.tail
			if (head._1 == 0) {
				val median = tree.QueryMedian()
				head._2.steps.foreach(step => {result(step) = median})
			}
			if (head._1 > 0) {
				tree.Update(head._2.branches(head._1 - 1)._1, false)
			}
			if (head._1 < head._2.branches.length) {
				stack = (head._1 + 1, head._2)::stack
				val branch = head._2.branches(head._1)
				stack = (0, branch._2)::stack
				tree.Update(branch._1, true)
			}
		}
		result
	}
	def main(args:Array[String]) {
		val T = Console.readLine.toInt
		val commands = new Array[Int](T)
		for (i <- 0 until T) {
			val command = Console.readLine.toInt
			commands(i) = command
		}
		// println("input done")
		val res = Solve(commands)
		for (i <- 1 to commands.length)
			println(res(i))
	}
}
