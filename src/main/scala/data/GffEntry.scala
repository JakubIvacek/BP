package data

/**
 * A case class representing an entry from a GFF file
 *
 * @param contig     The contig where the feature is located.
 * @param start      The start position
 * @param end        The end position
 * @param attributes A map of attributes associated with the feature
 */
case class GffEntry(
                     contig: String,
                     start: Int,
                     end: Int,
                     attributes: Map[String, String]
                   )
