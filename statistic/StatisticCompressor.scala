package compress.statistic

import compress.Compressor

/** A statistic compressor relies on the statistics of symbols in source
  * @param source the input source
  * @tparam S type of symbol used in source
  */
abstract class StatisticCompressor[S](source : Seq[S]) extends Compressor[S, Seq[Bit]]
  {
    /** A map giving the occurrences of each symbol in the source sequence */
    val occurrences : Map[S, Int] = source.groupBy(identity).map(x => (x._1, x._2.length))

    /** SHANNON entropy of source */
    val entropy : Double = occurrences.map(x => x._2/(source.length.doubleValue())).map(x => -x*math.log(x)/math.log(2)).sum

    /** The sequence of occurrences sorted by count */
    val orderedCounts : Seq[(S, Int)] = occurrences.toList.sortBy(x => x._2)

    /** The encoding tree (in most cases, depends from `source`) */
    def tree : Option[EncodingTree[S]]

    /** @inheritdoc */
    def compress(msg: Seq[S]): Seq[Bit] = tree match {
      case Some(value) => msg.map(x => value.encode(x)).flatten.flatten
      case None => Nil
    }

    /** @inheritdoc */
    def uncompress(res: Seq[Bit]): Option[Seq[S]] = tree match {
      case Some(value) => value.decode(res)
      case None => Option(Nil)
    }
  }

