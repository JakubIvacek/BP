package hgvs

/**
 * Object to map codons (triplet of nucleotides) to their amino acids.
 * Provides a function to retrieve the amino acid from a given codon.
 */
object CodonAmino {

  /**
   * A map that associates each possible codon to its amino acid.
   * - Keys are 3-letter codons (e.g., "ATA", "ATC", "TTT", etc.).
   * - Values are the amino acid abbreviations (e.g., "I" for Isoleucine, "M" for Methionine, "* "for Stop codon, etc.).
   * Map contains all the standard 64 codons, including the stop codons (TAA, TAG, TGA) represented by "*".
   */
  val codonToAminoAcid: Map[String, String] = Map(
    "ATA" -> "I", "ATC" -> "I", "ATT" -> "I", "ATG" -> "M", "ACA" -> "T", "ACC" -> "T",
    "ACG" -> "T", "ACT" -> "T", "AAC" -> "N", "AAT" -> "N", "AAA" -> "K", "AAG" -> "K",
    "AGC" -> "S", "AGT" -> "S", "AGA" -> "R", "AGG" -> "R", "CTA" -> "L", "CTC" -> "L",
    "CTG" -> "L", "CTT" -> "L", "CCA" -> "P", "CCC" -> "P", "CCG" -> "P", "CCT" -> "P",
    "CAC" -> "H", "CAT" -> "H", "CAA" -> "Q", "CAG" -> "Q", "CGA" -> "R", "CGC" -> "R",
    "CGG" -> "R", "CGT" -> "R", "GTA" -> "V", "GTC" -> "V", "GTG" -> "V", "GTT" -> "V",
    "GCA" -> "A", "GCC" -> "A", "GCG" -> "A", "GCT" -> "A", "GAC" -> "D", "GAT" -> "D",
    "GAA" -> "E", "GAG" -> "E", "GGA" -> "G", "GGC" -> "G", "GGG" -> "G", "GGT" -> "G",
    "TCA" -> "S", "TCC" -> "S", "TCG" -> "S", "TCT" -> "S", "TTC" -> "F", "TTT" -> "F",
    "TTA" -> "L", "TTG" -> "L", "TAC" -> "Y", "TAT" -> "Y", "TAA" -> "*", "TAG" -> "*",
    "TGC" -> "C", "TGT" -> "C", "TGA" -> "*", "TGG" -> "W", "CTA" -> "L", "CTC" -> "L",
    "CTG" -> "L", "CTT" -> "L"
  )

  /**
   * Given a codon (a string of three nucleotides), returns the corresponding amino acid.
   *
   * @param codon A string representing the 3-nucleotide codon (e.g., "ATA", "ATG").
   * @return The corresponding amino acid abbreviation (e.g., "I", "M"). If the codon is invalid, returns "X".
   */
  private def getAminoAcidFromCodon(codon: String): String = {
    // Use the codonToAminoAcid map to get the corresponding amino acid, defaulting to "X" for invalid codons.
    codonToAminoAcid.getOrElse(codon, "X")
  }

  /**
   * Translates a DNA sequence into its corresponding protein sequence.
   * Incomplete codons (less than 3 nucleotides) are ignored.
   *
   * @param dnaSequence A string representing the DNA sequence (e.g., "ATGGCC").
   * @return A string representing the protein sequence (e.g., "MA"). 
   */
  def translateDnaToProtein(dnaSequence: String): String = {
    dnaSequence
      .grouped(3) 
      .filter(_.length == 3) 
      .map(getAminoAcidFromCodon) 
      .mkString 
  }
}
