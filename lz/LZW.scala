package compress.lz

import compress.Compressor
import Dictionaries._


/** The LZW compression method
  * @param initialDictionary the starting dictionary
  */
class LZW(val initialDictionary : Dictionary = ASCII) extends Compressor[Char, Seq[Int]]
  {

    def compressBis(dictionaire: Map[String, Int], resultat : Seq[Int], chaine : Seq[Char], msgRestant : Seq[Char]) : Seq[Int] = {
      /*S'il n'y a plus rien à encoder (ie. que message restant est vide) on renvoie le resultat (ie. le message encodé)
      On ajoute également au résultat la chaine de caractère traité à l'apelle récursif précédent.
       */
      if (msgRestant.isEmpty) { if (chaine.isEmpty) resultat.reverse else ((dictionaire(chaine.toString())) +: resultat).reverse }
      else {
        /*Tant que le(s) caractères à encoder existe dans le dictionnaire, on ajoute un nouveau caractère, jusqu'à ce que notre String
        n'existe plus dans le dictionnaire.
         */
          if (dictionaire.contains(chaine + "" + msgRestant.head)) {compressBis (dictionaire, resultat, chaine + "" + msgRestant.head, msgRestant.tail)}
          /*Si le mot n'existe pas dans le dictionnaire, alors on met à jours le dictionnaire en rajoutant le mot privé de son dernier caractères
          on met à jour le résultat en rajoutant le code associé au mot privé de son dernier caractère au résultat
          et on renvoie un apelle récursif avec comme chaine de départ le dernier caractère qu'on a enelvé
           */
          else {
            val newDictionnaire: Map[String, Int] = dictionaire ++ List ((chaine + "" + msgRestant.head, dictionaire.size) ).toMap
            val newResultat: Seq[Int] = dictionaire (chaine.toString ()) +: resultat
            compressBis (newDictionnaire, newResultat, "" + msgRestant.head, msgRestant.tail)
          }
      }
    }

    /** @inheritdoc */
    def compress(msg : Seq[Char]) : Seq[Int] = {
      compressBis(initialDictionary.map(a => (a, a.charAt(0).toInt)).toMap, List[Int](), "", msg)
    }


    def uncompressBis(dictionaire: Map[Int, String], resultat : Seq[Char], nouveauMot : Option[(Int, String)], res : Seq[Int]) : Option[Seq[Char]] =
      if (res.isEmpty) Option(resultat.reverse.mkString("")) //S'il n'y a plus rien à décoder, on renvoie le resultat
      else {
      //Sinon, s'il y a encore des éléements à décoder
      // On regarde si le prochain element à décoder est dans le dictionnaire
      dictionaire.get(res.head) match {
        //Dans le cas où cette élément est dans le dictionnaire, apelons "mot" la valeur correspondate
        case Some(value) => {
          //On rajoute les caractères de l'appel précédent concatener au premier cractère de "mot" au dictionnaire
          //Si c'est notre premier apell (et donc il n'y a pas d'apelle précédent), on renvoie le même dictionnaire sans y ajouter de nouveaux mots
          val (newDictionnaire, newPositionDico) = nouveauMot match {
            case Some((positionDico, prefix)) => (dictionaire + (positionDico -> (prefix + value.head)), positionDico+1)
            case None => (dictionaire, dictionaire.size)
          }
          //Puis on relance un apelle avec le dicttionaire mis à jour, l'ancien resultat concatener au "mot",
          // on met à jour les cractères de l'apelle précédent en les remplacant par les caractères du "mot",
          // et on renvoie le reste des elemnts à décoder
          uncompressBis(newDictionnaire, value.reverse ++: resultat, Some(newPositionDico -> value), res.tail)
        }
        //Dans le cas où cette élément n'est pas dans le dictionnaire
          //On regarde si l'on a déja fait un appel précédent
        case None => nouveauMot match {
            //Si un appel a déjà était fait
          case Some((positionDico, prefix)) => {
            //On verifie que le numéro encoder n'est que supérieur d'un indice (c'est le cas où on a un retard dans la maj du dictionnaire
            if (dictionaire.size == res.head) {
              //Cela signifie que le mot à ajouter est composé de lui même + son premier element (exemple "aa" + "a" = "aaa")
              val newChaine = prefix + prefix.head
              val newDictionnaire = dictionaire + (positionDico -> newChaine)
              //On met à jour le dictionnaire, le résultat, le début du prochain mot ainsi que la liste des caractères restants à décoder
              uncompressBis(newDictionnaire, newChaine ++: resultat, Some((positionDico+1) -> newChaine), res.tail)
		        }
            //Si l'indice ne correspon pas, alors c'est une erreur d'encodage et on renvoie None
		        else {None}
          }
            //Si aucun appel n'a était fait, on renvoie None car il y a une erreur d'encodage
          case None => None
        }
      }
    }
    
    /** @inheritdoc */
    def uncompress(res : Seq[Int]) : Option[Seq[Char]] = uncompressBis(initialDictionary.map(a => (a.charAt(0).toInt, a)).toMap, Nil, None, res)

  }


