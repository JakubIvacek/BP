package data

import scala.collection.mutable

/**
 * A class representing an interval tree that allows efficient storage and searching of genomic intervals
 * The tree is organized by contigs allows searching for overlapping, upstream, or downstream
 */
class IntervalTree {
  private val tree = mutable.Map[String, mutable.TreeSet[GffEntry]]()
  implicit val ordering: Ordering[GffEntry] = Ordering.by(_.start)

  def insert(entry: GffEntry): Unit = {
    tree.getOrElseUpdate(entry.contig, mutable.TreeSet[GffEntry]()).add(entry)
  }

  def search(contig: String, position: Int): List[GffEntry] = {
    tree.get(contig) match {
      case Some(entries) =>
        entries.filter(entry => entry.start <= position && entry.end >= position).toList
      case None => List()
    }
  }

  def findClosestUpstream(contig: String, position: Int): Option[GffEntry] = {
    tree.get(contig) match {
      case Some(entries) =>
        entries.filter(_.end < position).maxByOption(_.end)
      case None => None
    }
  }

  def findClosestDownstream(contig: String, position: Int): Option[GffEntry] = {
    tree.get(contig) match {
      case Some(entries) =>
        entries.filter(_.start > position).minByOption(_.start)
      case None => None
    }
  }
}
