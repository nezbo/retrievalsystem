package dk.nezbo.ir.ass1

import ch.ethz.dal.tinyir.io.TipsterStream
import ch.ethz.dal.tinyir.processing.XMLDocument
import ch.ethz.dal.tinyir.processing.Tokenizer
import scala.collection.mutable.ListBuffer
import com.github.aztek.porterstemmer.PorterStemmer
import scala.collection.mutable.PriorityQueue
import scala.Ordering

object Main  {

  def main(args: Array[String]) {
    // prepare queries
    val queries = args.map(q => Tokenizer.tokenize(q.toLowerCase()).map(PorterStemmer.stem(_))).toList
    println(queries)
    
    val t0 = System.nanoTime()
    val tipster = new TipsterStream ("./tipster/zips/")  
    println("Number of files in zips = " + tipster.length)
    
    val t1 = System.nanoTime()
    println("Time elapsed: "+(t1-t0)/1000000000.0+" s")

    var i = 0
    //val tfs = ListBuffer.empty[(Int,Map[String,Int])]
    val topscores = queries.map(q => new PriorityQueue[(String,Double)]()(Ordering.by(ordering)))
    
    for (doc <- tipster.stream.take(1000)) {
      if(i % 1000 == 0) println(i+" files done.")
      
      // DO THINGS HERE
      //tfs += ((doc.ID,getTermFrequencies(doc)))
      for(i <- 0 until queries.length){
        
        topscores(i).enqueue((doc.name,getTermScore(doc,queries(i))))
      }
      
      i += 1
    }
    println(topscores.map(t => t.take(10)))
    val t2 = System.nanoTime()
    println("\nTime elapsed: "+(t2-t0)/1000000000.0+" s")
  }
  
  def getTermScore(doc : XMLDocument, qterms : List[String]) : Double = {
    val tfs = getTermFrequencies(doc)
    
    val qtfs = qterms.flatMap(q => tfs.get(q))
	val numTermsInCommon = qtfs.filter(_ > 0).length
	val docEuLen = tfs.values.map(x => x * x).sum.toDouble
	val queryLen = qterms.length.toDouble
	val termOverlap = qtfs.sum / (docEuLen * queryLen)
	numTermsInCommon + termOverlap
  }
  
  def getTermFrequencies(doc : XMLDocument) : Map[String,Int] = {
    doc.tokens.map(PorterStemmer.stem(_)).groupBy(identity).mapValues(v => v.length)
  }
  
  def ordering(row : (String,Double)) = row._2 
}