package compress.statistic

/** The HUFFMAN compression method */
class Huffman[S](source : Seq[S]) extends StatisticCompressor[S](source)
  {
    /** @inheritdoc */
    lazy val tree : Option[EncodingTree[S]] = {

      /** Cette fonction renvoie la somme du nombre d'apparitions des symboles qui comppose cette arbre
        * @param arbre un arbre
        * @return renvoie la somme du nombre d'apparitions des symboles qui comppose cette arbre
        */
      def weight[S](arbre : EncodingTree[S]):Int = arbre match {
        case EncodingNode(lbl, _, _) => lbl
        case EncodingLeaf(lbl, _) =>lbl
      }

      /** Cette fonction permet d'inserer un arbre dans une lsite d'abre le tout en conservant l'ordre décroissant en nb d'occurences
        * @param arbre_inserer un arbre à insérer
        * @param seq la liste d'arbre qui est supposé trié par ordre décroissant
        * @return renvoie l'arbre insérer dans la liste
        */
      def inserer[S](arbre_inserer: EncodingTree[S], seq : Seq[EncodingTree[S]] ): Seq[EncodingTree[S]] = {
        if (seq.isEmpty || weight(arbre_inserer) > weight(seq.head) ) {arbre_inserer +: seq}
        else {seq.head +: inserer(arbre_inserer, seq.tail)}
      }

      /** Cette fonction permet de réaliser les fusions permettant d'aboutir à l'arbre d'Huffman
        * @param liste_tree la liste de feuille triès par rodre décroissant par rapport au nombres d'occurences
        * @return renvoie l'arbre d'Huffman
        */
      def fusion[S](liste_tree : Seq[EncodingTree[S]]) : EncodingTree[S] = {
        if ( liste_tree.length == 1 ) {liste_tree.head}
        else
        {
          val longueur = liste_tree.length -1;
          fusion(inserer(EncodingNode(weight(liste_tree(longueur-1))+weight(liste_tree(longueur)), liste_tree(longueur-1), liste_tree(longueur)), liste_tree.dropRight(1).dropRight(1) ))
        }
      }

      val OrderedLeaf: Seq[EncodingTree[S]] = orderedCounts.reverse.map(leaf => EncodingLeaf(leaf._2,leaf._1 ));

      if ( source.isEmpty) {None} else {Option(fusion(OrderedLeaf))}
    }
}