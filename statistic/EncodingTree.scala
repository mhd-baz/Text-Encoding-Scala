package compress.statistic

import scala.annotation.tailrec

/** Trait for binary encoding trees (integer-labeled binary trees)
 *
  * @tparam S type of symbols (in leaves only)
  */
sealed abstract class EncodingTree[S](val label : Int)
  {
    /* OPERATIONS ON VALUES */

    /** Checks if tree contains given value
      * @param x value to search
      * @return true if the tree has a leaf with value `x`
      */
    def has(x : S) : Boolean = this match {
      case EncodingNode(_, left, right) => left.has(x) || right.has(x)
      case EncodingLeaf(_, value) => value==x
    }

    /** Reduce operation on tree values when applying a function on leaves beforehand
      * @param f the function applied to each leaf value
      * @param op the aggregation operation on a node
      * @tparam U the result type of `f`
      * @return the aggregated value of the tree
      */
    def reduceWith[U](f : S => U)(op : (U, U) => U) : U = this match
      {
        case EncodingLeaf(_, v   ) => f(v)
        case EncodingNode(_, l, r) => op((l reduceWith f)(op), (r reduceWith f)(op))
      }

    /** Reduce operation on tree values
      *
      * `t reduce op` is a shorthand for `(t reduceWith {v => v})(op)`
      * @param op the aggregation operation on a node
      * @return the aggregated value of the tree
      */
    def reduce(op : (S, S) => S) : S = (this reduceWith {v => v})(op)


    /* OPERATIONS ON LABELS */

    /** Reduce operation on tree labels when applying a function on leaves beforehand
      * @param fL the function applied to each leaf label
      * @param opL the aggregation operation on a node : first parameter is the node label
      * @tparam A the result type of `f`
      * @return the result of aggregation operation recursively applied to tree
      */
    def reduceLabelWith[A](fL : Int => A)(opL : (Int, A, A) => A) : A = this match
      {
        case EncodingLeaf(lbl, _   ) => fL(lbl)
        case EncodingNode(lbl, l, r) => opL(lbl, (l reduceLabelWith fL)(opL), (r reduceLabelWith fL)(opL))
      }

    /** Reduce operation on tree labels
      *
      * `t reduceLabel opL` is a shorthand for `(t reduceLabelWith {lbl => lbl})(opL)`
      * @param opL the aggregation operation on a node : first parameter is the node label
      * @return the aggregated label of the tree
      */
    def reduceLabel(opL : (Int, Int, Int) => Int) : Int = (this reduceLabelWith identity)(opL)


    /* ENCONDING/DECODING OPERATIONS */

    /** Fonction auxiliaire qui permet de renvoyer le code binaire d'une valeur x sosu forme de Seq[Bit]
      * @param x value to encode
      * @param code la liste qui contiendra le code binaire
      * @return the corresponding bit sequence of `x` is a leaf of encoding tree
      */
    def encodeBIS(x : S, code : Seq[Bit]) : Seq[Bit] = this match {
      case EncodingNode(_, left, right) => if (left.has(x)) {left.encodeBIS(x, code:+Zero)} else {right.encodeBIS(x, code:+One)}
      case EncodingLeaf(_, _) => code
    }

    /** Computes the bit sequence corresponding to a tentative leaf value.
      * @param x value to encode
      * @return the corresponding bit sequence of `x` is a leaf of encoding tree, `None` otherwise
      */
    def encode(x : S) : Option[Seq[Bit]] = if (!has(x)) None else {
      this match {
        case EncodingNode(label, left, right) => Option(this.encodeBIS(x, List()))
        case EncodingLeaf(label, value) => Option(List(Zero))
      }
    }


    /** Computes the next value corresponding to the beginning of bit sequence (if possible)
      * @param res the bit sequence to decode
      * @return the decoded value and the bit sequence left to be decoded
      */
    def decodeOnce_Bis(res : Seq[Bit]): Option[S] =
      this match {
      case EncodingNode(_, left, right) => if (res.isEmpty) None else {
        if (res.head == Zero) left.decodeOnce_Bis(res.tail) else {right.decodeOnce_Bis(res.tail)}
      }
      case EncodingLeaf(_, value) => if (!res.isEmpty) None else { Option(value) }
    }

    /** Computes the next value corresponding to the beginning of bit sequence (if possible)
      * @param res the bit sequence to decode
      * @return the decoded value and the bit sequence left to be decoded or `None` if current bit sequence does not lead to a leaf in enconding tree
      */
    def decodeOnce(res : Seq[Bit]) : Option[(S, Seq[Bit])] = this match {
      case EncodingNode(_, _, _) => decodeOnce_Bis(res) match {
        case Some(value) => Option(value, res)
        case None => None
      }
      case EncodingLeaf(_, value) => if (res.head == Zero) Option(value, res) else None
    }


    /** Computes the sequence of values from the sequence of bits
      * @param res the bit sequence to decode
      * @return the sequence of decoded values or `None` otherwise
      */
    def decode(res : Seq[Bit]) : Option[Seq[S]] = {
      @tailrec
      def decode_Bis(tete: Seq[Bit], res : Seq[Bit], result : Seq[S]) : Option[Seq[S]] = decodeOnce(tete) match {
        case Some((s, _)) => if (res.isEmpty) Option(result++:List(s)) else {decode_Bis(List(res.head), res.tail, result++:List(s))}
        case None => if (res.isEmpty) None else {decode_Bis(tete:+res.head, res.tail, result)}
      }
      if (res.isEmpty) None else {decode_Bis(List(res.head), res.tail, List())}
    }

    /* MISCELLANEOUS */

    /** Fonction qui permet de calculer la somme des multiplciation de nombres de bits necessaire pour coder un caractère par son nombre d'apparition*/
    def meanLengthBis(compteur : Double) : Double = this match {
      case EncodingNode(label, left, right) => left.meanLengthBis(compteur+1) + right.meanLengthBis(compteur+1)
      case EncodingLeaf(label, _) => label*compteur
    }

    /** Mean length of code associated to encoding tree */
    lazy val meanLength : Double = this match {
      case EncodingNode(_, left, right) => (left.meanLengthBis(1) + right.meanLengthBis(1))/this.label
      case EncodingLeaf(_, _) => 1
    }


    /** @inheritdoc */
    override def toString : String = this match
     {
       case EncodingLeaf(lbl, v   ) => (v, lbl).toString()
       case EncodingNode(lbl, l, r) => s"EncodingNode([$lbl], $l, $r)"
     }
  }
case class EncodingNode[S](override val label : Int, left : EncodingTree[S], right : EncodingTree[S]) extends EncodingTree[S](label)
case class EncodingLeaf[S](override val label : Int, value : S) extends EncodingTree[S](label)
