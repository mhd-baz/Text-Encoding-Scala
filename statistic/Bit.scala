package compress.statistic

import scala.annotation.tailrec

/** Implementation of a bit */
sealed trait Bit
case object Zero extends Bit
case object One  extends Bit

