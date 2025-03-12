package data

/**
 * A case class representing an entry from a UniProt-PDB mapping file.
 *
 * @param pdbId      The PDB identifier associated with the structure (e.g., "3C5W").
 * @param method     The experimental method used to determine the structure (e.g., "X-ray", "EM").
 * @param resolution The resolution of the structure in Ångström, if available (e.g., 2.80). Wrapped in Option.
 * @param chain      The chain identifier within the PDB structure (e.g., "A", "B", "D").
 * @param start      The start position of the amino acid range covered by the structure.
 * @param end        The end position of the amino acid range covered by the structure.
 */

case class UniProtEntry(
                         pdbId: String,
                         method: String,
                         resolution: Option[Double],
                         chain: String,
                         start: Int,
                         end: Int,
                         uniProtId: String
                       )
