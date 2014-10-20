package dk.nezbo.ir.ass1

import ch.ethz.dal.tinyir.processing.XMLDocument

/**
 * The general 'traint' (interface) if the relevance models, such
 * that they can be used in the same context.
 */
trait RelevanceModel {
  /**
   * The method to process a list of documents to search for a number of queries, giving
   * a list with a list of relevant documents for each query.
   */
  def process(queries : Seq[Seq[String]], docs : Iterator[XMLDocument]): Seq[Seq[String]]
}