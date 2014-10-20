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
import scala.util.Try

/**
 * A simple class containing the method to run the processing
 * of queries with the settings defined by the static main-method.
 */
private class Mainx {
  def run = {
	// load topics
    val topics = Main.loadTopics.drop(39)
    Main.debug(topics)
    
    // Prepare queries
    val queries = topics.map(_._1).map(q => Tokenizer.tokenize(q.toLowerCase()).map(Main.getStem(_))).toList
    Main.debug(queries)
    
    // Open my own implementation of reading the files
    val t0 = System.nanoTime()
    val tipster = new Utility.EmilParse(Main.data_folder+"/zips")
    
    val t1 = System.nanoTime()
    Main.debug("Time elapsed: "+(t1-t0)/1000000000.0+" s")

    // Let the selected relevance model process the documents with the queries.
	val topscores = Main.rel_model.process(queries, tipster.stream.take(Main.num_documents))
	topscores.foreach(Main.debug(_))
    
    val t2 = System.nanoTime()
    Main.debug("\nTime elapsed: "+(t2-t0)/1000000000.0+" s")
    
    // Compare relevance
    val quality = new PrecRecInterpolation(11)
    val judgements = new TipsterGroundTruth(Main.data_folder+"/qrels").judgements
    val avgP = new ListBuffer[Double]()
    
    // for printing to file
    val file = new File(Main.data_folder+"/results/"+Main.output_filename)
    file.getParentFile().mkdir()
    
    for(topic <- topics.zipWithIndex){
      val id = topic._1._2
      val name = topic._1._1
      val index = topic._2
      
      if(judgements.contains(id.toString)){ // the topic has been judged
        // look at judgements and compare
    	println("\nEvaluating: "+name)
      
	    val ranked = topscores(index)
	    val relev = judgements.get(id.toString).get.toSet
	      
	    val precRecall = PrecisionRecall.evaluate(ranked.toSet, relev)
	    println(precRecall)
	    val avgPInterp = quality.nAveragedPrecision(ranked, relev)
	    avgP += avgPInterp
	    println("Average Interpolated Precision: "+avgPInterp)
      } else { // not judged = new one
        // Print the top N documents for each query to the file
        val fw = new FileWriter(file,true)
        topscores(index).toList.zipWithIndex.foreach(l => fw.write(id+" "+(l._2+1)+" "+l._1 +"\n"))
        fw.close()
      }
    }
    
    // Mean Average (interpolated) Precision
    if(!avgP.isEmpty){
	  val map = avgP.sum / avgP.length
      println("\nOverall Mean Average Precision: "+map)
    }
    
    // Final time taken.
    val t3 = System.nanoTime()
    Main.debug("\nTime elapsed: "+(t3-t0)/1000000000.0+" s")
  }
}

/**
 * The singleton containing the main method which parses
 * any given parameters and sets those settings for the
 * actual run.
 */
object Main  {
  
  var data_folder = "./tipster"
  var output_folder = "./tipster/result/"
  var output_filename = "ranking-l-emil-jacobsen.run"
    
  var num_to_find = 100
  val num_documents = Int.MaxValue 
  var debug_print = true
  var rel_model : RelevanceModel = new LanguageModel()//new TermFrequencyModel()

  /*
   * Command line structure:
   * scala emil.jar [data folder] [relevance model] [result size (n)] [debug]
   * 
   * Values above are default values:
   * It expects folder "/zips/" and files "qrels" and "topics" in the data folder.
   * 
   * Example: "scala emil.jar "./tipster" "l" "100" "false"
   */
  def main(args: Array[String]) {
    // set parameters
    if(args.size > 0) data_folder = args(0)
    if(args.size > 1) 
      if(args(1).toLowerCase.equals("l")) {
        rel_model = new LanguageModel()
        output_filename = "ranking-l-emil-jacobsen.run"
      } else {
        rel_model = new TermFrequencyModel()
        output_filename = "ranking-t-emil-jacobsen.run"
      }
    if(args.size > 2) num_to_find = Try(args(2).toInt).getOrElse(100)
    if(args.size > 3) debug_print = Try(args(3).toBoolean).getOrElse(true)
    
    // run!
    new Mainx().run
  }
   
  /**
   * A method for loading the topics (with IDs) from the topics file.
   * Expected to be called "topics" in the data_folder.
   */
  def loadTopics : List[(String,Int)] = {
    val topics = Source.fromFile(data_folder+"/topics").getLines.filter(l => l.contains("<title>")).map(t => t.split(":").last.trim)
    val ids = Source.fromFile(data_folder+"/topics").getLines.filter(l => l.contains("<num>")).map(t => t.split(":").last.trim.toInt)
    
    topics.zip(ids).toList
  }
  
  /**
   * Prints the toString value of the given object to the std_out
   * if the debug variable is set to true.
   */
  def debug(obj : Any) = if(Main.debug_print) println(obj)
  
  /**
   * The ordering of a (DocID,Score) tuple.
   */
  def ordering(row : (String,Double)) = -row._2
  
  /**
   * Extracts term frequencies for the words contained in the
   * given document.
   */
  def getTermFrequencies(doc : XMLDocument) : Map[String,Int] = {
	doc.tokens.map(getStem(_)).groupBy(identity).mapValues(v => v.length)
  }
  
  // cache to reduce time stemming common words, cleared often to limit memory usage
  val stemCache : HashMap[String,String] = new HashMap[String,String]
  
  /**
   * Retrieves the stem of the word if it has already been
   * calculated, or calculated from scratch and cached.
   */
  def getStem(word : String) : String = {
    if(stemCache.size > 500000) stemCache.clear
    
    if(!stemCache.contains(word)){
      stemCache.put(word, PorterStemmer.stem(word))
    }
    stemCache(word)
  }
}