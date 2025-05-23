package hgvs

import anotation.VariantTypeAnnotation.getProteinSequence
import data.VariantType.{DEL, DUP, EXT, FS, INDEL, INS, INV, Other, RPT, SNP}
import data.{DnaVariant, GffEntry, VariantType}
import files.FastaReaderSW

object HGVSp {
  /**
   * HGVSp generates part after the coordinate for protein-level annotation, including position and sequence part.
   * This function processes a genetic variant and returns the corresponding HGVS protein notation.
   *
   * @param variant         The genetic variant (e.g., SNP, DEL, INS, INDEL, etc.).
   * @param refProtein      The reference protein sequence.
   * @param altProtein      The alternate protein sequence.
   * @param variantOffset   The offset of the variant within the sequence, used to calculate the amino acid position.
   * @param strandPlus      A boolean indicating whether the variant is on the plus strand (true) or minus strand (false).
   * @param cdsSeqLen       The length of the CDS (coding sequence), which is used for certain mutation types (e.g., frameshift).
   * @return A tuple with the HGVS protein notation (position and sequence part).
   */
  def returnProteinHGVS(variant: DnaVariant, refProtein: String, altProtein: String, variantOffset: Int, strandPlus: Boolean, cdsSeqLen: Int): (String, String) = {
    val aaIndex = variantOffset / 3
    if (aaIndex * 3 >= refProtein.length || aaIndex * 3 >= altProtein.length) {
      //println(s"${aaIndex * 3} is out of bounds for proteins: ref=${refProtein.length}, alt=${altProtein.length}")
      return ("", "")
    }
    variant.proteinVarType match {
      case SNP => handleSNP(aaIndex, refProtein, altProtein)
      case DEL => handleDEL(aaIndex, variant, refProtein, altProtein, strandPlus)
      case INS => handleINS(aaIndex, variant, refProtein, altProtein, strandPlus)
      case INDEL => handleINDEL(aaIndex, variant, refProtein, altProtein, strandPlus)
      case RPT => handleRPT(aaIndex, refProtein, altProtein)
      case DUP => handleDUP(aaIndex, variant, refProtein, altProtein, strandPlus)
      case EXT => handleExtension(aaIndex, variant, refProtein, altProtein, strandPlus)
      case FS  => handleFS(aaIndex, refProtein, altProtein, cdsSeqLen)
      case _ => ("", "")
    }
  }

  /**
   * Substitution: This function handles (SNP) mutations and generates the corresponding HGVS notation.
   * GENERATES PART AFTER COORDINATE p.
   *
   * Syntax for substitution:
   *   - Experimentally ascertained protein consequence: sequence_identifier ":p." aa_position alternate_base
   *     Example: NP_003997.1:p.Trp24Cys
   *     NP_003997.1:p.Trp24Ter
   *     NP_003997.1:p.W24*
   *
   * @param aaIndex    Index of the amino acid where the SNP occurs in the protein.
   * @param refProtein The reference protein sequence.
   * @param altProtein The alternate protein sequence with the SNP.
   * @return A tuple with the HGVS substitution notation and the alternate amino acid (or "Ter" if a stop codon is present).
   */
  def handleSNP(aaIndex: Int, refProtein: String, altProtein: String): (String, String) = {
    val refAAList = refProtein.grouped(3).toList
    val altAAList = altProtein.grouped(3).toList
    if (aaIndex < refAAList.length && aaIndex < altAAList.length) {
      val refAA = refAAList(aaIndex)
      val altAA = altAAList(aaIndex) match {
        case aa => aa
      }

      val pos = s"$refAA${aaIndex + 1}"
      (pos, altAA)
    } else {
      ("?", "?")
    }
  }

  /**
   * Deletion: This function handles deletion mutations and generates the corresponding HGVS notation.
   * GENERATES PART AFTER COORDINATE p.
   *
   * Syntax for deletion:
   *   - Single position: sequence_identifier ":p." aa_position "del"
   *     Example: NP_003997.2:p.Val7del
   *   - Position range: sequence_identifier ":p." aa_position "_" aa_position "del"
   *     Example: NP_003997.2:p.Lys23_Val25del
   *
   * @param aaIndex    Index of the amino acid where the deletion occurs in the alternate protein.
   * @param variant    The genetic variant representing the deletion.
   * @param refProtein The reference protein sequence.
   * @param strandPlus Boolean indicating if the strand is the plus strand.
   * @return A tuple with the HGVS deletion notation and an empty string (as the deleted sequence is not represented).
   */
  def handleDEL(aaIndex: Int, variant: DnaVariant, refProtein: String, altProtein: String, strandPlus: Boolean): (String, String) = {

    // Group the reference protein into amino acids
    val refAminoAcids = refProtein.grouped(3).toList
    val altAminoAcids = altProtein.grouped(3).toList

    // Calculate the number of amino acids deleted based on the length difference
    val delAAcount = (  variant.refAllele.length - variant.altAllele.length ) / 3
    if (delAAcount == 1) {
      // Single amino acid deletion
      val refAA = refAminoAcids(aaIndex) // Get the amino acid at aaIndex
      val pos = s"$refAA${aaIndex + 1}"
      (pos, "")

    } else if (delAAcount > 1) {
      // Multi-amino acid deletion (range)
      val startIndex = if (strandPlus) aaIndex else aaIndex - delAAcount + 1
      val endIndex = if (strandPlus) aaIndex + delAAcount - 1 else aaIndex

      // Ensure valid indices
      if (startIndex < 0 || endIndex >= refAminoAcids.length) return ("", "")

      // Extract the range of amino acids for deletion
      val deletedAAs = refAminoAcids.slice(startIndex, endIndex + 1)
      val startAA = deletedAAs.head
      val endAA = deletedAAs.last

      val pos = s"$startAA${startIndex + 1}_${endAA}${endIndex + 1}"
      (pos, "")

    } else {
      ("", "")
    }
  }

  /**
   * Insertion: This function handles insertion mutations and generates the corresponding HGVS notation.
   * GENERATES PART AFTER COORDINATE p.
   *
   * Syntax for insertion:
   * sequence_identifier ":p." aa_range "ins" sequence
   * Example:
   * NP_004371.2:p.(Pro46_Asn47insSerSerTer)
   *
   * @param aaIndex    Index of the amino acid where the insertion occurs in the alternate protein.
   * @param variant    The genetic variant representing the insertion.
   * @param refProtein The reference protein sequence.
   * @param altProtein The alternate protein sequence with the insertion.
   * @param strandPlus Boolean indicating if the strand is the plus strand.
   * @return A tuple with the HGVS insertion notation and the inserted sequence.
   */
  def handleINS(aaIndex: Int, variant: DnaVariant, refProtein: String, altProtein: String, strandPlus: Boolean): (String, String) = {
    val insLength = (variant.altAllele.length - variant.refAllele.length) / 3
    if (insLength > 0 && aaIndex < refProtein.length / 3) {
      // Define the start and end adjacent residues
      val leftAA = refProtein.slice(aaIndex * 3, aaIndex * 3 + 3)
      val rightAA = refProtein.slice((aaIndex + 1) * 3, (aaIndex + 1) * 3 + 3)

      val insertedSeq = altProtein.slice(aaIndex * 3 + 3, aaIndex * 3 + 3 + insLength * 3)
      val pos = s"$leftAA${aaIndex + 1}_$rightAA${aaIndex + 2}"
      (pos, s"$insertedSeq")
    } else {
      ("", "")
    }
  }

  /**
   * Deletion-Insertion (delins): This function handles deletion-insertion mutations and generates the
   * corresponding HGVS notation.
   * GENERATES PART AFTER COORDINATE p.
   *
   * Syntax for single position delins:
   * sequence_identifier ":p." aa_position "delins" sequence
   * Examples:
   * NP_004371.2:p.Asn47delinsSerSerTer
   *
   * Syntax for position range delins:
   * sequence_identifier ":p." aa_position "_" aa_position "delins" sequence
   * Example:
   * NP_003070.3:p.Glu125_Ala132delinsGlyLeuHisArgPheIleValLeu
   *
   * @param aaIndex    Index of the amino acid where the indel occurs in the alternate protein.
   * @param variant    The genetic variant representing the deletion and insertion.
   * @param refProtein The reference protein sequence.
   * @param altProtein The alternate protein sequence with the indel.
   * @param strandPlus Boolean indicating if the strand is the plus strand.
   * @return A tuple with the HGVS delins notation and the inserted sequence.
   */
  def handleINDEL(aaIndex: Int, variant: DnaVariant, refProtein: String, altProtein: String, strandPlus: Boolean): (String, String) = {
    val insLength = (variant.altAllele.length - variant.refAllele.length) / 3
    val delLength = (variant.refAllele.length - variant.altAllele.length) / 3

    if (delLength > 0 || insLength > 0) {
      val (startIndex, endIndex) = if (strandPlus) {
        (aaIndex, aaIndex + delLength - 1)
      } else {
        (aaIndex - delLength + 1, aaIndex)
      }

      val startAA = refProtein.substring(startIndex * 3, startIndex * 3 + 3)
      val endAA = if (endIndex < refProtein.length / 3)
        refProtein.substring(endIndex * 3, endIndex * 3 + 3)
      else
        ""

      val insertedSeq = altProtein.slice(aaIndex * 3, aaIndex * 3 + insLength * 3)

      val pos = if (delLength == 1) {
        s"$startAA${startIndex + 1}"
      } else {
        s"$startAA${startIndex + 1}_$endAA${endIndex + 1}"
      }

      (pos, s"delins$insertedSeq")
    } else {
      ("", "")
    }
  }

  /**
   * Duplication: This function handles both single position and position range mutations and generates the
   * corresponding HGVS notation.
   * GENERATES PART AFTER COORDINATE p.
   *
   * Syntax for N-terminus extension:
   * sequence_identifier ":p.Met1ext" new_initiation_site
   * Example: NP_003997.2:p.Met1ext-5
   *
   *
   * @param aaIndex    Index of the amino acid where the extension occurs in the reference protein.
   * @param variant    The DnaVariant
   * @param refProtein The reference protein sequence.
   * @param altProtein The alternate protein sequence with the extension.
   * @param strandPlus Boolean indicating whether the variant is on the plus strand
   * @return A tuple with the HGVS extension notation and any additional sequence information.
   */
  def handleDUP(aaIndex: Int, variant: DnaVariant, refProtein: String, altProtein: String, strandPlus: Boolean): (String, String) = {
    val insLength = (variant.altAllele.length - variant.refAllele.length) / 3

    if (insLength > 0) {
      val startAA = refProtein.substring(aaIndex * 3, aaIndex * 3 + 3)
      val pos = s"$startAA${aaIndex + 1}"

      // If the duplication involves a range
      val endIndex = aaIndex + insLength - 1
      if (endIndex > aaIndex) {
        val endAA = refProtein.substring(endIndex * 3, endIndex * 3 + 3)
        (s"$startAA${aaIndex + 1}_${endAA}${endIndex + 1}", "")
      } else {
        (pos, "")
      }
    } else {
      ("", "")
    }
  }

  /**
   * Repeated sequence: This function handles repeated sequence mutations and generates the corresponding HGVS notation.
   * GENERATES PART AFTER COORDINATE p.
   *
   * Syntax for repeated sequences:
   * sequence_identifier ":p." position sequence "[" total_copy_number "]"
   * Examples:
   * NP_0123456.1:p.Ala2[10]
   * NP_0123456.1:p.Arg65_Ser67[12]
   *
   * @param aaIndex    Index of the amino acid where the repeat occurs in the alternate protein.
   * @param refProtein The reference protein sequence.
   * @param altProtein The alternate protein sequence with the repeat.
   * @return A tuple with the HGVS repeated sequence notation and the total repeat count in brackets.
   */
  def handleRPT(aaIndex: Int, refProtein: String, altProtein: String): (String, String) = {
    val remaining = altProtein.substring(refProtein.length)

    // Find the smallest repeat unit that constructs the remaining sequence
    val repeatUnit = (1 to remaining.length).find { unitLength =>
      val candidateUnit = remaining.substring(0, unitLength)
      remaining.grouped(unitLength).forall(_ == candidateUnit)
    }.map(remaining.substring(0, _)).getOrElse("")

    if (repeatUnit.isEmpty) {
      return ("?", "?")
    }

    val totalRepeatCount = altProtein.length / repeatUnit.length

    // Handle range repeats
    if (repeatUnit.length > 3 || remaining.contains(repeatUnit)) {
      val startPos = aaIndex + 1 // Protein position (starting from 1)
      val endPos = startPos + (totalRepeatCount - 1)
      val pos = s"${repeatUnit}${startPos}_${repeatUnit}${endPos}"
      return (pos, s"[$totalRepeatCount]")
    }

    // Handle single amino acid repeats
    val pos = s"$repeatUnit${aaIndex + 1}"
    (pos, s"[$totalRepeatCount]")
  }

  /**
   * Extension: This function handles both N-terminal and C-terminal extensions and generates the
   * corresponding HGVS notation.
   * GENERATES PART AFTER COORDINATE p.
   *
   * Syntax for N-terminus extension:
   * sequence_identifier ":p.Met1ext" new_initiation_site
   * Example: NP_003997.2:p.Met1ext-5
   *
   * Syntax for C-terminus extension:
   * sequence_identifier ":p.Ter" aa_position aa "extTer" extension_length
   * Example: NP_003997.2:p.Ter110GlnextTer17
   *
   * @param aaIndex    Index of the amino acid where the extension occurs in the reference protein.
   * @param variant    The DnaVariant
   * @param refProtein The reference protein sequence.
   * @param altProtein The alternate protein sequence with the extension.
   * @param strandPlus Boolean indicating whether the variant is on the plus strand
   * @return A tuple with the HGVS extension notation and any additional sequence information.
   */
  def handleExtension(aaIndex: Int, variant: DnaVariant, refProtein: String, altProtein: String, strandPlus: Boolean): (String, String) = {
    val extensionLength = (altProtein.length - refProtein.length) / 3

    if (extensionLength > 0) {
      if (strandPlus) {
        // C-terminal extension
        if (altProtein.startsWith(refProtein)) {
          val endAA = refProtein.substring(refProtein.length - 3, refProtein.length)
          return (s"Ter${aaIndex + 1}${endAA}extTer", s"$extensionLength")
        }

        // N-terminal extension
        if (altProtein.endsWith(refProtein)) {
          return ("Met1ext", s"-$extensionLength")
        }
      } else {
        // C-terminal extension on the minus strand
        if (altProtein.startsWith(refProtein)) {
          val endAA = refProtein.substring(refProtein.length - 3, refProtein.length)
          return (s"Ter${aaIndex + 1}${endAA}extTer", s"$extensionLength")
        }

        // N-terminal extension on the minus strand
        if (altProtein.endsWith(refProtein)) {
          return (s"Met1ext", s"-$extensionLength")
        }
      }
    }
    ("", "")
  }

  /**
   * Frameshift: This function handles frameshift mutations and generates the corresponding HGVS notation.
   * GENERATES PART AFTER COORDINATE p.
   *
   * Syntax for frameshift with known stop codon position:
   * sequence_identifier ":p." aa_position "fsTer" position
   * Example: NP_0123456.1:p.Arg97ProfsTer23
   *
   * @param aaIndex           Index of the amino acid where the frameshift occurs in the alternate protein.
   * @param refProtein        The reference protein sequence.
   * @param altProtein        The alternate protein sequence with the frameshift.
   * @param cdsSequenceLength The length of the CDS (coding sequence) for calculating termination position.
   * @return A tuple with the HGVS frameshift notation and any additional sequence information.
   */
  def handleFS(aaIndex: Int, refProtein: String, altProtein: String, cdsSequenceLength: Int): (String, String) = {
    val refAA = refProtein.substring(aaIndex * 3, aaIndex * 3 + 3)
    val affectedPos = aaIndex + 1

    // Find stop codon (Ter position)
    val remainingAAs = altProtein.drop(aaIndex * 3).grouped(3).toList
    val stopIndex = remainingAAs.indexWhere(_ == "Ter")
    // Determine stop position if found
    val stopPosition: Option[Int] = if (stopIndex >= 0) Some(affectedPos + stopIndex) else None
    stopPosition match {
      case Some(pos) => (s"$refAA$affectedPos", s"Ter$pos")
      case None => (s"$refAA$affectedPos", "")
    }
  }
}
