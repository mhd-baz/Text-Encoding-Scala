package compress.statistic


/** The SHANNON-FANO compression method */
class ShannonFano[S](source : Seq[S]) extends StatisticCompressor[S](source)
  {
    /** @inheritdoc */
    lazy val tree : Option[EncodingTree[S]] = {

      /** Fnction qui permet de renvoyer environ la moitié d'une liste selon le nombres d'occurences de ces elemts */
      def splitSelonOcc(accG : Int, accD : Int, gauche : Seq[(S, Int)], droite : Seq[(S, Int)]) : (Seq[(S, Int)], Seq[(S, Int)]) = {
        (accD > accG) match {
          case true => splitSelonOcc(accG+droite.head._2, accD-droite.head._2, gauche :+ droite.head, droite.tail)
          case false => (gauche, droite)
        }
      }

      /** Fonction qui renvoie l'arbre binaire associé à une séquence à encoder
        * @param liste liste des occurences d'appartitions des éléments de la séquence à encoder trié dans l'ordre décroissant
        * @return s'il n'y a qu'un seul élément dans la lsite renvoie une feuille, sinon un ensemble de noeuds et feuilles
        */
      def arbreBinaire(liste : Seq[(S, Int)]) : EncodingTree[S] = liste.length match {
        case 1 => EncodingLeaf(liste.head._2, liste.head._1) //Si un seul elmts, l'arbre n'est composé que d'une feuilles
        case n => {
          val (left, right) = splitSelonOcc(0, liste.map(x => x._2).sum, List(), liste) //On coupe la liste en deux parties quasi égales selon les occurences
          EncodingNode(liste.map(x => x._2).sum, arbreBinaire(left), arbreBinaire(right));
          /* et on crée un noeud avec à gauche la partie gauche de la liste (la partie où il y a les plus grandes occurences)
          et à droite la liste de droite (la partie où il y a les moins grandes occurences)*/
        }
      }
      if (orderedCounts.length == 0) {None} else {Option(arbreBinaire(orderedCounts.reverse))}
    }
  }

