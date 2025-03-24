package data

/**
 * Represents a VCF variant entry.
 *
 * @param chrom The chromosome of the variant.
 * @param pos The position of the variant.
 * @param id The ID of the variant.
 * @param ref The reference allele.
 * @param alt The alternate allele.
 * @param qual The quality score.
 * @param filter The filter status.
 * @param info Additional information about the variant.
 */
case class VCFEntry(chrom: String, pos: Int, id: String, ref: String, alt: String, qual: String, filter: String, info: String)