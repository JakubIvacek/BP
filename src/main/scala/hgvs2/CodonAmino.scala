package hgvs2

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
    "ATA" -> "Ile", "ATC" -> "Ile", "ATT" -> "Ile", "ATG" -> "Met",
    "ACA" -> "Thr", "ACC" -> "Thr", "ACG" -> "Thr", "ACT" -> "Thr",
    "AAC" -> "Asn", "AAT" -> "Asn", "AAA" -> "Lys", "AAG" -> "Lys",
    "AGC" -> "Ser", "AGT" -> "Ser", "AGA" -> "Arg", "AGG" -> "Arg",
    "CTA" -> "Leu", "CTC" -> "Leu", "CTG" -> "Leu", "CTT" -> "Leu",
    "CCA" -> "Pro", "CCC" -> "Pro", "CCG" -> "Pro", "CCT" -> "Pro",
    "CAC" -> "His", "CAT" -> "His", "CAA" -> "Gln", "CAG" -> "Gln",
    "CGA" -> "Arg", "CGC" -> "Arg", "CGG" -> "Arg", "CGT" -> "Arg",
    "GTA" -> "Val", "GTC" -> "Val", "GTG" -> "Val", "GTT" -> "Val",
    "GCA" -> "Ala", "GCC" -> "Ala", "GCG" -> "Ala", "GCT" -> "Ala",
    "GAC" -> "Asp", "GAT" -> "Asp", "GAA" -> "Glu", "GAG" -> "Glu",
    "GGA" -> "Gly", "GGC" -> "Gly", "GGG" -> "Gly", "GGT" -> "Gly",
    "TCA" -> "Ser", "TCC" -> "Ser", "TCG" -> "Ser", "TCT" -> "Ser",
    "TTC" -> "Phe", "TTT" -> "Phe", "TTA" -> "Leu", "TTG" -> "Leu",
    "TAC" -> "Tyr", "TAT" -> "Tyr", "TAA" -> "Stop", "TAG" -> "Stop",
    "TGC" -> "Cys", "TGT" -> "Cys", "TGA" -> "Stop", "TGG" -> "Trp",
    "CTA" -> "Leu", "CTC" -> "Leu", "CTG" -> "Leu", "CTT" -> "Leu"
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
