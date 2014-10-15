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
import java.io.File
import org.w3c.dom.Document
import ch.ethz.dal.tinyir.lectures.TipsterGroundTruth
import dk.nezbo.ir.ass1.Utility.PrecRecInterpolation
import ch.ethz.dal.tinyir.lectures.PrecisionRecall
import java.io.FileWriter
import java.io.PrintWriter
import scala.collection.mutable.HashMap

object Main  {
  
  val input_folder = "./tipster/zips"
  val output_filename = "ranking-emil-jacobsen.run"
    
  val num_to_find = 100
  val num_documents = Int.MaxValue 
  val debug_print = true
  val rel_model = new LanguageModel()//new TermFrequencyModel()////

  def main(args: Array[String]) {
    // load topics
    val topics = loadTopics.take(10) //.drop(39)
    debug(topics)
    
    // prepare queries
    val queries = topics.map(_._1).map(q => Tokenizer.tokenize(q.toLowerCase()).map(getStem(_))).toList
    debug(queries)
    
    val t0 = System.nanoTime()
    //val tipster = new TipsterStream ("./tipster/zips/")
    val tipster = new Utility.EmilParse(input_folder)
    //debug("Number of files in zips = " + tipster.length)
    
    val t1 = System.nanoTime()
    debug("Time elapsed: "+(t1-t0)/1000000000.0+" s")

	val topscores = rel_model.process(queries, tipster.stream.take(num_documents))
    
    val t2 = System.nanoTime()
    debug("\nTime elapsed: "+(t2-t0)/1000000000.0+" s")
    
    // Compare relevance
    val quality = new PrecRecInterpolation(11)
    val judgements = new TipsterGroundTruth("tipster/qrels").judgements
    val avgP = new ListBuffer[Double]()
    
    // for printing
    val file = new File("tipster/results/"+output_filename)
    file.getParentFile().mkdir()
    
    for(topic <- topics.zipWithIndex){
      val id = topic._1._2
      val name = topic._1._1
      val index = topic._2
      
      if(judgements.contains(id.toString)){
        // look at judgements and compare
    	println("\nEvaluating: "+name)
      
	    val ranked = topscores(index)
	    val relev = judgements.get(id.toString).get.toSet
	      
	    val precRecall = PrecisionRecall.evaluate(ranked.toSet, relev)
	    println(precRecall)
	    val avgPInterp = quality.nAveragedPrecision(ranked, relev)
	    avgP += avgPInterp
	    println("Average Interpolated Precision: "+avgPInterp)
      } else {
        // print wanted output
        val fw = new FileWriter(file,true)
        topscores(index).toList.zipWithIndex.foreach(l => fw.write(id+" "+(l._2+1)+" "+l._1 +"\n"))
        fw.close()
      }
    }
    
    // Mean Average Precision
    if(!avgP.isEmpty){
	  val map = avgP.sum / avgP.length
      println("\nOverall Mean Average Precision: "+map)
    }
    
    val t3 = System.nanoTime()
    debug("\nTime elapsed: "+(t3-t0)/1000000000.0+" s")
  }
  
  def loadTopics : List[(String,Int)] = {
    val lines = Source.fromFile("tipster/topics").getLines
    val topics = lines.filter(l => l.contains("<title>")).map(t => t.split(":").last.trim)
    val ids = lines.filter(l => l.contains("<num>")).map(t => t.split(":").last.trim.toInt)
    
    topics.zip(ids).toList
  }
  
  def debug(obj : Any) = if(debug_print) println(obj)
  
  def ordering(row : (String,Double)) = -row._2
  
  def getTermFrequencies(doc : XMLDocument) : Map[String,Int] = {
	doc.tokens.map(getStem(_)).groupBy(identity).mapValues(v => v.length)
  }
  
  val stemCache : HashMap[String,String] = new HashMap[String,String]
  def getStem(word : String) : String = {
    if(stemCache.size > 500000) stemCache.clear
    
    if(!stemCache.contains(word)){
      stemCache.put(word, PorterStemmer.stem(word))
    }
    stemCache(word)
  }
}