import compress.RLE
import compress.lz.{LZ78, LZW}
import compress.statistic.{Bit, Huffman, One, ShannonFano, Zero}



object TestOral {
  def main(args: Array[String]): Unit = {
    /*
    println("---------- RLE ----------")
    println(" ")
    println("1) S : Char")
    println(" ")
    val exempleRLE_Char : RLE[Char] = new RLE[Char]
    val msg0 = "" //Message vide
    val msg1 = "AAAABBCCBB" //Message non vide
    println("msg0 : ", msg0)
    println("compression msg0 : ", exempleRLE_Char.compress(msg0))
    println("décompression msg0 : ", exempleRLE_Char.uncompress(exempleRLE_Char.compress(msg0)))
    println("msg1 : ", msg1)
    println("compression msg1 : ", exempleRLE_Char.compress(msg1))
    println("décompression msg1 : ", exempleRLE_Char.uncompress(exempleRLE_Char.compress(msg1)))
    println(" ")
    println("2) S : Bit")
    println(" ")
    val exempleRLE_Bit = new RLE[Bit]
    val msg2 : Seq[Bit] = List(One, One, One, One, Zero, Zero, One, Zero, Zero, Zero)
    println("msg2 : ", msg2)
    println("compression msg2 : ", exempleRLE_Bit.compress(msg2))
    println("décompression msg2 : ", exempleRLE_Bit.uncompress(exempleRLE_Bit.compress(msg2)))
    println(" ")
    println("3) Encodage corrompu")
    val encodage_corrompu = List((One,4), (Zero,2), (One,-1), (Zero,3))
    println("encodage corrompu : ", encodage_corrompu)
    println("décodage de l'encodage corrompu : ", exempleRLE_Bit.uncompress(encodage_corrompu))
    println(" ")
    println(("-------------------------"))
    println(" ")
    */
    /*
    println("---------- STATISTIC ----------")
    println(" ")
    println("1) SHANNON FANO")
    println(" ")
    val msg0 = "HOURRAHOURRAHOURRRRA" // HOURRA HOURRA HOURRRRA
    val shannon0 : ShannonFano[Char] = new ShannonFano(msg0)
    println("msg0 : ", msg0)
    println("Occurences classé par ordre décroissant : ", shannon0.orderedCounts.reverse)
    println("L'entropie du msg0 : ", shannon0.entropy)
    println("Le nombre moyen de bites pour encoder un caractere : ", shannon0.tree.get.meanLength)
    println("L'arbre : ", shannon0.tree.get)
    println("L'encodage : ", shannon0.compress(msg0))
    println("Le décodage : ", shannon0.uncompress(shannon0.compress(msg0)))
    println(" ")
    val encodage_errone = List(One, Zero, Zero, Zero, Zero, One, One, One)
    println("Encodage erronée : ", encodage_errone)
    println("Decodage erronée : ", shannon0.uncompress(encodage_errone))
    println(" ")
    val msg1 = "AAAA"
    val shannon1 : ShannonFano[Char] = new ShannonFano(msg1)
    println("Occurences classé par ordre décroissant : ", shannon1.orderedCounts)
    println("L'entropie du msg1 : ", shannon1.entropy)
    println("Le nombre moyen de bites pour encoder un caractere : ", shannon1.tree.get.meanLength)
    println("L'arbre : ", shannon1.tree.get)
    println("L'encodage : ", shannon1.compress(msg1)) //A = Zero
    println("Le décodage : ", shannon1.uncompress(shannon1.compress(msg1)))
    println(" ")
    val encodage_errone1 = List(Zero, Zero, Zero, One)
    println("Encodage erronée : ", encodage_errone1)
    println("Decodage erronée : ", shannon1.uncompress(encodage_errone1))
    println(" ")
    println("2) HUFFMAN")
    println(" ")
    val msg2 : Seq[String] = List("H", "O", "U", "R", "R", "A", "H", "O", "U", "R", "R", "A", "H", "O", "U", "R", "R", "R", "R", "A") // HOURRA HOURRA HOURRRRA
    val huffman0 : Huffman[String] = new Huffman[String](msg2)
    println("msg2 : ", msg2)
    println("Occurences classé par ordre décroissant : ", huffman0.orderedCounts.reverse)
    println("L'entropie du msg0 : ", huffman0.entropy)
    println("Le nombre moyen de bites pour encoder un caractere : ", huffman0.tree.get.meanLength)
    println("L'arbre : ", huffman0.tree.get)
    println("L'encodage : ", huffman0.compress(msg2))
    println("Le décodage : ", huffman0.uncompress(huffman0.compress(msg2)))
    println(" ")
    val msg3 : Seq[Bit] = List()
    val huffman1 : Huffman[Bit] = new Huffman[Bit](msg3)
    println("msg3 : ", msg3)
    println("Occurences classé par ordre décroissant : ", huffman1.orderedCounts.reverse)
    println("L'entropie du msg0 : ", huffman1.entropy)
    println("Le nombre moyen de bites pour encoder un caractere : ", huffman1.tree match {
      case Some(value) => value.meanLength
      case None => None
    })
    println("L'arbre : ", huffman1.tree match {
      case Some(value) => value
      case None => None
    })
    println("L'encodage : ", huffman1.compress(msg3))
    println("Le décodage : ", huffman1.uncompress(huffman1.compress(msg3)))
    println(" ")
    println("-------------------------")
    println(" ")
    /*val msg4 = "HOUAARRRRRRRR"
    val shannon3 : ShannonFano[Char] = new ShannonFano(msg4)
    println(shannon3.orderedCounts.reverse)
    println(shannon3.tree)*/
    */
    /*
    println("---------- DICTIONNAIRES ----------")
    println(" ")
    println(" ")
    println("1) LZ78")
    println(" ")
    val msg0 = "belle echelle !"
    println("msg0 : ", msg0)
    println("L'encodage : ", LZ78.compress(msg0))
    println("Le décodage : ", LZ78.uncompress(LZ78.compress(msg0)))
    println(" ")
    val encodage_errone0 = List((7,'b'), (0,'e'), (0,'l'), (3,'e'), (0,' '), (2,'c'), (0,'h'), (2,'l'), (4,' '), (0,'!'))
    println("Encodage erronée : ", encodage_errone0)
    println("Decodage erronée : ", LZ78.uncompress(encodage_errone0))
    println(" ")
    val msg1 = ""
    println("msg1 : ", msg1)
    println("L'encodage : ", LZ78.compress(msg1))
    println("Le décodage : ", LZ78.uncompress(LZ78.compress(msg1)))
    println(" ")
    println("2) LZW")
    println(" ")
    val lzw : LZW = new LZW()
    println("msg0 : ", msg0)
    println("L'encodage : ", lzw.compress(msg0))
    println("Le décodage : ", lzw.uncompress(lzw.compress(msg0)))
    println(" ")
    val msg2 = "bbba"
    println("msg2 : ", msg2)
    println("L'encodage : ", lzw.compress(msg2))
    println("Le décodage : ", lzw.uncompress(lzw.compress(msg2)))
    println(" ")
    val encodage_errone1 = List(98, 256, 297)
    println("Encodage erronée : ", encodage_errone1)
    println("Decodage erronée : ", lzw.uncompress(encodage_errone1))
    println(" ")
    println(("---------------------------------"))
    println(" ")
    */
  }

}
