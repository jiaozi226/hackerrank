import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap;
class Item(val features:Array[Int], var labelId:Int);
class BayesModel(val wordArr:Array[String],
						val wordMap:HashMap[String,Int],
						val labelArr:Array[String],
						val labelMap:HashMap[String,Int],
						val matrix:Array[Array[Double]],
						val labelProb:Array[Double]);
object Solution{
	def stem(word:String):String ={
		val default = "";
		if (word.forall(_.isDigit))
			"special:iamanumber"
		else
			word.toLowerCase.filter(!_.isDigit)
	}
	def parseInput(desc:String, wordArr:Array[String], wordMap:HashMap[String,Int]): Item = {
		val words = desc.filter(ch => {ch != '(' && ch != ')'}).trim.split(' ').map(stem).filter(! _.isEmpty);
		val features = new Array[Int](wordArr.length);
		words.foreach(word=>{
			if (wordMap.contains(word)) {
				val wid = wordMap(word);
		  	features(wid) = features(wid) + 1;
			}
		});
		new Item(features, -1);
	}
	def predict(model:BayesModel, item:Item):(Int,String) = {
		val probArr = new Array[Double](model.labelArr.length);
		model.labelArr.indices.foreach(i => {
			var prob:Double = 0.0;
			item.features.zipWithIndex.foreach(p =>{
				prob = prob + p._1 * scala.math.log(model.matrix(i)(p._2));
			});
			prob = prob + scala.math.log(model.labelProb(i));
			probArr(i) = prob;
		});
		val idx= probArr.zipWithIndex.reduce((a,b) => {
			if (a._1 > b._1) a else b
		})._2;
		(idx,model.labelArr(idx))
	}
	def train(fname:String):BayesModel = {
		// label arr and map
		val labelMap = new HashMap[String, Int]();
		val labelArr = Array(
		"axe deo"
		,"best-seller books"
		,"calvin klein"
		,"camcorder"
		,"camera"
		,"chemistry"
		,"chromebook"
		,"c programming"
		,"data structures algorithms"
		,"dell laptops"
		,"dslr canon"
		,"mathematics"
		,"nike-deodrant"
		,"physics"
		,"sony cybershot"
		,"spoken english"
		,"timex watch"
		,"titan watch"
		,"tommy watch"
		,"written english");
		labelArr.zipWithIndex.map(pair => labelMap += (pair._1 -> pair._2));

		// Build up wordtable and items with features
		var wordArr = new ArrayBuffer[String](0);
		val wordMap = new HashMap[String,Int]();
		for (line <- Source.fromFile(fname).getLines) {
		  var arr = line.split('\t');
			if (arr.length == 2) {
				val a = arr match {case Array(a,b) =>(a,b)}
				val words = a._1.split(' ').map(stem).filter(!_.isEmpty);
				words.foreach(word=>{
					if (!wordMap.contains(word)) {
					  wordArr = wordArr.+:(word);
						wordMap += (word-> (wordArr.length-1));
					}
				})
			}
		}

		//  Build features and labels into items
		var itemArr = new ArrayBuffer[Item](0);
		for (line <- Source.fromFile(fname).getLines) {
			val arr = line.trim.split('\t')
			if (arr.length == 2) {
				val a = arr match {case Array(a,b) =>(a,b)}
				val item = parseInput(a._1, wordArr.toArray, wordMap);
				item.labelId = labelMap(a._2);
				itemArr = itemArr.+:(item);
			}
		}
		
		// Train
		val matrix = new Array[Array[Double]](labelArr.length);
		labelArr.indices.foreach(idx=>{
			matrix(idx) = new Array[Double](wordArr.length);
		});
		val wordCountForLabel = new Array[Int](labelArr.length);
		itemArr.foreach(item=>{
			item.features.foreach(count => {
				wordCountForLabel(item.labelId) = wordCountForLabel(item.labelId) + count;
			});
		});
		itemArr.foreach(item => {
			item.features.zipWithIndex.foreach(p => {
				matrix(item.labelId)(p._2) += p._1;
			});
		});
		matrix.indices.foreach(i=>{
			matrix(i).indices.foreach(j => {
				// alpha for smoothing
				val alpha:Double = 1.0;
				matrix(i)(j) = (matrix(i)(j) + 1 * alpha) / (wordCountForLabel(i) + wordArr.length*alpha);
			});
		});
		val labelProb = new Array[Double](labelArr.length);
		itemArr.foreach(item => {
			labelProb(item.labelId) = labelProb(item.labelId) + 1;
		});
		labelProb.indices.foreach(i => {
			labelProb(i) = labelProb(i) / itemArr.length;
		});

		val model = new BayesModel(wordArr.toArray, wordMap, labelArr.toArray, labelMap, matrix, labelProb)

		// Predict on training set
    var t:Int = 0;
	  itemArr.foreach(item => {
			val res = predict(model, item);
			if (res._1 == item.labelId) {
			  t = t + 1;
			}
		});
		// println( 1.0 * t / itemArr.length);
		model
	}
  def main(args:Array[String]) {
		val model = train("training.txt");
		// predict
		val lineNum = Console.readLine.trim.toInt;
		for (i <- 0 until lineNum) {
			val desc = Console.readLine();
			val item = parseInput(desc, model.wordArr, model.wordMap);
			val res = predict(model, item);
			println(res._2);
		}
	}
}
