import com.github.tototoshi.csv._
import java.io._
import scala.collection.mutable.HashMap
import scala.collection.mutable.Set
import util.control.Breaks._

object BeckerVowelCorpus {
  def main(args:Array[String]): Unit = {
    val phonemes: HashMap[String, Set[(String, String, String, String, String)]]= load()
    val samples: Set[(String, String, String, String, String)] = Set()
    phonemes.foreach(samples ++= _._2)
    val languages: Set[String] = Set()
    samples.foreach (languages += _._1)

    // expected 53, got 68 ?
    println(s"${phonemes.size} distinct phonemes")
    // expected 223, got 231 ?
    println(s"${languages.size} distinct languages")
    // nothing expected, got 3694
    println(s"${samples.size} distinct vowel/language samples")
  }

  def load(): HashMap[String, Set[(String, String, String, String, String)]] = {
    println("loading Becker vowel corpus...")

    val reader = CSVReader.open(new File("src/main/resources/BeckerVowelCorpus.csv"))
    val headers::entries = reader.all()
    reader.close()

    val headerIndexes = headers.zipWithIndex.toMap

    // Key: IPA notation
    // Value: Set(language, F1, F2, F3, F4)
    val phonemes: HashMap[String, Set[(String, String, String, String, String)]] = HashMap()

    for (e <- entries) {
      val language = e.apply(headerIndexes("Language"))

      // anywhere from 1..14 vowels
      for (v <- 1 to 14) breakable {
        val phonemeOS = e.apply(headerIndexes(s"V${v}OS"))
        val phonemePS = e.apply(headerIndexes(s"V${v}PS"))
        val f1 = e.apply(headerIndexes(s"V${v}F1"))
        val f2 = e.apply(headerIndexes(s"V${v}F2"))
        val f3 = e.apply(headerIndexes(s"V${v}F3"))
        val f4 = e.apply(headerIndexes(s"V${v}F4"))

        if (phonemePS.isEmpty && phonemeOS.isEmpty) {
          break
        } else if (!phonemePS.isEmpty && !phonemeOS.isEmpty && phonemePS != phonemeOS) {
          println(s"[WARN]\tOS and PS defined but not equal for ${language} V${v} (OS=${phonemeOS}, PS=${phonemePS}) using OS")
        }

        val phoneme = if (!phonemeOS.isEmpty) phonemeOS else phonemePS
        val phonemeEntries = phonemes.getOrElse(phonemeOS, Set())
        phonemeEntries.add(language, f1, f2, f3, f4)
        phonemes += (phonemeOS -> phonemeEntries)
      }
    }

    return phonemes
  }
}
