package compress.lz

import compress.Compressor

/** The LZ78 compression method */
object LZ78 extends Compressor[Char, Seq[(Int, Char)]]
  {

    def compressBis(chaineCandidate : (Int, String), indexList : Seq[String], resultat: Seq[(Char, Int)], msgRestant : Seq[Char]) : Seq[(Int, Char)] = {
      if (msgRestant.isEmpty) (resultat ++ List((chaineCandidate._2.reverse.head, chaineCandidate._1)) ).map(x => (x._2, x._1))
      else {
        indexList.indexOf(chaineCandidate._2) match {
          case -1 =>
            compressBis((0, msgRestant.head.toString),  indexList :+ chaineCandidate._2 ,resultat ++ List((chaineCandidate._2.reverse.head, chaineCandidate._1)), msgRestant.tail)
          case value=> compressBis( (value+1, chaineCandidate._2 :+ msgRestant.head), indexList, resultat, msgRestant.tail)
        }
      }
    }

    /** @inheritdoc */
    def compress(msg : Seq[Char]) : Seq[(Int, Char)] = msg.isEmpty match {
      case true => List.empty[(Int, Char)]
      case false => compressBis((0, msg.head.toString) , List(), List.empty, msg.tail)
    }



    def uncompressBis(chaineCandidate : (Int, Char), indexList : Seq[String], codeRestant: Seq[(Int, Char)], resultat : Seq[Char]) : Option[Seq[Char]] = {
      chaineCandidate._1 match {
        case 0 => {
          val newResultat = resultat :+ chaineCandidate._2
          codeRestant.isEmpty match {
            case true => Option(newResultat.mkString(""))
            case false =>uncompressBis(codeRestant.head, indexList :+ chaineCandidate._2.toString, codeRestant.tail, newResultat)
          }
        }
        case value =>
          if ((value > indexList.length) | (value < 0)) {None}
          else{
            val chaineAajouter = indexList(value-1) :+ chaineCandidate._2
            val newResultat = resultat :++ chaineAajouter
            codeRestant.isEmpty match {
              case true =>Option(newResultat.mkString(""))
              case false => uncompressBis(codeRestant.head, indexList :+  chaineAajouter, codeRestant.tail, newResultat)
            }
        }
      }
    }

    /** @inheritdoc */
    def uncompress(res : Seq[(Int, Char)]) : Option[Seq[Char]] = res.isEmpty match {
      case true => Option("")
      case false => uncompressBis((res.head._1, res.head._2), List(),  res.tail , "")
    }
  }
