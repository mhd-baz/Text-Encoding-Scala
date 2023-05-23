package compress

import scala.annotation.tailrec

/** The Run-length encoding compression method */
class RLE[T] extends Compressor[T, Seq[(T, Int)]]
  {

    /** Fonction Compress RLE dans le cas où le msg n'est pas vide
      */
    def compresseBis(value: (T, Int), msg: Seq[T], code : Seq[(T, Int)]) : Seq[(T, Int)] = msg.length match {
      case 0 => code :++ List(value)
      case _ => {
        if (msg.head == value._1) compresseBis((value._1, value._2+1), msg.tail, code)
        else compresseBis((msg.head, 1), msg.tail, code :++ List(value))
      }
    }

    /** @inheritdoc */
    def compress(msg : Seq[T]) : Seq[(T, Int)] = msg.length match {
      case 0 => List()
      case _ => compresseBis((msg.head, 1), msg.tail, List())
    }


    /** Fonction UNcompress RLE dans le cas où le msg n'est pas vide
      */
    def uncompresseBis(value: (T, Int), code : Seq[(T, Int)], msg: Seq[T]) : Option[Seq[T]] = {
      if (value._2 == 1) {
        if (code.length == 0) {Option(msg :+ value._1)}
        else {uncompresseBis(code.head, code.tail, msg :+ value._1)}
      }
      else { if (value._2 < 1) {None}
      else {uncompresseBis((value._1, value._2-1), code, msg :+ value._1)}
      }
    }

    /** @inheritdoc */
    def uncompress(seq : Seq[(T, Int)]) : Option[Seq[T]] =  seq.length match {
      case 0 => Option(List())
      case _ => uncompresseBis(seq.head, seq.tail, List())
    }
  }
