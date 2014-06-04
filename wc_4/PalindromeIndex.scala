object Solution {
	def solve(chars:Array[Char]) : Int = {
		var mid = chars.length / 2;
		// check for the first pass
		var ok = true;
		for (idx <- 0 until mid) {
			if (chars(idx) != chars(chars.length - idx - 1)) {
				ok = false;
			}
		}
		if (ok) {
			return -1;
		}
		// println("failed first pass");
		// check for the second pass, assuming remove some char on left
		mid = if (chars.length % 2 == 0) {mid} else {mid + 1}
		var checkCnt = chars.length - mid;
		var right= mid;
		var left = if (chars.length % 2 == 0) {mid} else {mid - 1};
		var diffCnt:Int = 0;
		var diffPoint = -1;
		while (left >= 0 && right < chars.length) {
		  if (chars(left) == chars(right)) {
				left = left - 1;
				right = right + 1;
			} else {
				diffPoint = left;
				diffCnt = diffCnt + 1;
				left = left - 1;
			}
		}
		if (diffCnt == 0) {
			return 0;
		} else if (diffCnt == 1) {
			return diffPoint;
		}
		// Check for the third pass, assuming remove some char on right
		mid = mid - 1;
		left = mid;
		right= if (chars.length % 2 == 0) {mid} else {mid + 1}
		checkCnt = mid + 1;
		diffCnt = 0;
		diffPoint = -1;
		while(left >=0 && right < chars.length) {
			if (chars(left) == chars(right)) {
				left = left - 1;
				right = right + 1;
			} else {
				diffPoint = right;
				right = right + 1;
				diffCnt = diffCnt + 1;
			}
		}
		if (diffCnt == 0) {
			return chars.length - 1;
		} else if (diffCnt == 1) {
			return diffPoint;
		} else {
			0  // Impossible branch actually
		}
	}
	def main(args:Array[String]) = {
		val caseNum = Console.readLine.trim.toInt;
		for (i  <- 0 until caseNum) {
			println(solve(Console.readLine.trim.toArray))
		}
	}
}
