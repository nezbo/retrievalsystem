package dk.nezbo.ir.ass1

import ch.ethz.dal.tinyir.io.ZipDirStream
import ch.ethz.dal.tinyir.processing.TipsterParse
import java.io.File
import java.io.InputStream
import ch.ethz.dal.tinyir.io.ZipStream
import ch.ethz.dal.tinyir.processing.XMLDocument
import scala.collection.mutable.Queue

object Utility {
  
  def main(args : Array[String]) {
    val inter = new PrecRecInterpolation(11)
    val result = inter.nAveragedPrecision((1 to 10), Set(2,3,6,8))
    println("\nResult: "+result)
    
    val result2 = inter.nAveragedPrecision((1 to 3), Set(1,2,3))
    println("\nResult: "+result2)
    
    val result3 = inter.nAveragedPrecision((1 to 20), Set(1,2,3,8,15))
    println("\nResult: "+result3)
    
    val result4 = inter.nAveragedPrecision((1 to 20), Set(21,22))
    println("\nResult: "+result4)
  }

class PrecRecInterpolation(n : Int) {
  var levels:List[Double] = (0 to n-1).map(i => i/(n-1).toDouble).toList
  
  def nAveragedPrecision[A](ranked : Seq[A], relev : Set[A]) : Double = {
    println("Ranked: "+ranked)
    println("Relevant: "+relev)
    
    val prec:Seq[Double] = this.relev(ranked,relev)
    		.zipWithIndex
    		.map{ case (rnk,rel) => (rel+1)/(rnk+1).toDouble}
    
    println("Relev loc: "+this.relev(ranked,relev))
    println("Prec: "+prec)
    
    val interp = this.interp(prec).toList
    val precAtN = levels.map(n => this.precAt(n,interp))
    println(precAtN)
    precAtN.sum / n
  }
  
  private def precAt(recall : Double, interp : Seq[Double]) : Double = {
    val n = Math.max(recall2num(recall,interp.size),1)
    interp(n-1)
  }
  
  // Returns a recall number [0.0,1.0] as a number between 0 
  // and the total number of relevant docs
  private def recall2num(recall : Double, num : Int) : Int = {
    Math.min((recall * num).ceil.toInt,num)
  }
  
  // Finds the indexes (in the ranked list) of relevant items
  private def relev[A](ranked : Seq[A], relev : Set[A]) = {
    ranked.zipWithIndex.filter{ case (a,_) => relev(a)}.map(_._2)
  }
  
  private def interp(p : Seq[Double]) : Seq[Double] = {
    if(p.length < 1) { 
      List(0.0)
      } else {
        p.scanRight(0.0)((a,b) => Math.max(a, b)).dropRight(1)
    }
    
  }
}

// one file at a time
class EmilParse(dirpath : String) {
  val ziplist = new File(dirpath).listFiles.filter(f => f.isDirectory())
  	.flatMap(f => f
      .listFiles.filter(_.getName.endsWith(".zip")))
      .union(new File(dirpath).listFiles.filter(_.getName.endsWith(".zip")))
  	  .map(z => z.getAbsolutePath).sorted.toList
  	  
  var queue = ziplist.drop(1).toList // the Queue class didn't dequeue properly :S
  
  var curZip = new ZipStream(ziplist(0)).stream.iterator
  var curFile = fetch

  def fetch : InputStream = {
    if(!curZip.hasNext){
      if(!queue.isEmpty){
        curZip = new ZipStream(queue(0)).stream.iterator
        queue = queue.drop(1).toList
        //println("next file - "+queue.length+" more to go.")
      }else{
        //println("no more files.")
        return null // done
      }
    }
    curZip.next
  }
  
  //  stream-ish
  val stream = new Iterator[XMLDocument] {
	  var i = -1
	  var hasNext = true
	  def next(): XMLDocument = 
	  { 
	    val result = curFile
	    curFile = fetch
	    hasNext = (curFile != null)
	    i += 1
	    new TipsterParse(result)
	  }
  }	
}
}