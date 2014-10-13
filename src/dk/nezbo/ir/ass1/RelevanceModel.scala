package dk.nezbo.ir.ass1

import ch.ethz.dal.tinyir.processing.XMLDocument

trait RelevanceModel {
  def process(queries : Seq[Seq[String]], docs : Iterator[XMLDocument]): Seq[Seq[String]]
}