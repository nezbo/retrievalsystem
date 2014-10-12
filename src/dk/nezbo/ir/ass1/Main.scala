package dk.nezbo.ir.ass1

import ch.ethz.dal.tinyir.io.TipsterStream
import ch.ethz.dal.tinyir.processing.XMLDocument
import ch.ethz.dal.tinyir.processing.Tokenizer
import scala.collection.mutable.ListBuffer
import com.github.aztek.porterstemmer.PorterStemmer
import scala.collection.mutable.PriorityQueue
import scala.Ordering
import javax.xml.parsers.DocumentBuilderFactory
import scala.io.Source
import org.w3c.dom.Document
import ch.ethz.dal.tinyir.lectures.TipsterGroundTruth
import dk.nezbo.ir.ass1.Utility.PrecRecInterpolation
import ch.ethz.dal.tinyir.lectures.PrecisionRecall

object Main  {
  
  val num_to_find = 100

  def main(args: Array[String]) {
    // load topics
    val topics = loadTopics.drop(38).take(1)
    println(topics)
    
    // prepare queries
    val queries = topics.map(_._1).map(q => Tokenizer.tokenize(q.toLowerCase()).map(PorterStemmer.stem(_))).toList
    println(queries)
    
    val t0 = System.nanoTime()
    val tipster = new TipsterStream ("./tipster/zips/")  
    println("Number of files in zips = " + tipster.length)
    
    val t1 = System.nanoTime()
    println("Time elapsed: "+(t1-t0)/1000000000.0+" s")

    var i = 0
    var topscores = queries.map(q => new PriorityQueue[(String,Double)]()(Ordering.by(ordering)))
    
    for (doc <- tipster.stream.take(1000)) {
      if(i % 1000 == 0) println(i+" files done.")
      
      // DO THINGS HERE
      for(i <- 0 until queries.length){
        
        topscores(i).enqueue((doc.name,getTermScore(doc,queries(i))))
        if(topscores(i).length > num_to_find)
          topscores(i).dequeue
      }
      
      i += 1
    }
    topscores = topscores.map(i => i.reverse)
    topscores.foreach(println(_))
    
    val t2 = System.nanoTime()
    println("\nTime elapsed: "+(t2-t0)/1000000000.0+" s")
    
    // Compare relevance
    val quality = new PrecRecInterpolation(11)
    val judgements = new TipsterGroundTruth("tipster/qrels").judgements
    
    for(topic <- topics.zipWithIndex){
      val id = topic._1._2
      val name = topic._1._1
      val index = topic._2
      
      if(judgements.contains(id)){
    	  println("Evaluating: "+name)
      
	      val ranked = topscores(index).map(d => d._1).toList
	      val relev = judgements.get(id).get.toSet
	      
	      val precRecall = PrecisionRecall.evaluate(ranked.toSet, relev)
	      println(precRecall)
	      val interpolatedScore = quality.nAveragedPrecision(ranked, relev)
	      println("Score: "+interpolatedScore+"\n")
      } else {
        // print wanted output
        topscores(index).toList.zipWithIndex.foreach(l => println(id+" "+(l._2+1)+" "+l._1._1))
      }
    }
    
    val t3 = System.nanoTime()
    println("\nTime elapsed: "+(t3-t0)/1000000000.0+" s")
  }
  
  def loadTopics : List[(String,Int)] = {
    val lines = Source.fromFile("tipster/topics").getLines
    val topics = lines.filter(l => l.contains("<title>")).map(t => t.split(":").last.trim)
    val ids = lines.filter(l => l.contains("<num>")).map(t => t.split(":").last.trim.toInt)
    
    topics.zip(ids).toList
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
  
  def ordering(row : (String,Double)) = -row._2 
}