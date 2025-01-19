package data

/**
 * A case class representing an entry from a GFF file
 *
 * @param contig     The contig where the feature is located.
 * @param start      The start position
 * @param end        The end position
 * @param strandPlus Boolean if strand + , -
 * @param name       Name of strand (gene, transcript, exon , CDS)
 * @param attributes A map of attributes associated with the feature
 */
case class GffEntry(
                     contig: String,
                     start: Int,
                     end: Int,
                     strandPlus: Boolean,
                     name: String,
                     attributes: Map[String, String]
                   )
