package quinemccluskey

import quinemccluskey._
import scala.collection.mutable.ListBuffer

object Test {
  def main(args: Array[String]) {
    var inputs = List[Term](new Term("0000"), new Term("0001"), new Term("0010"), new Term("0011"), new Term("0101"), new Term("0111"), new Term("1000"), new Term("1010"), new Term("1100"), new Term("1101"), new Term("1111"))
    val f = new Formula(inputs)
    val result = Formula.simplify(f)
    println(result)
  }
}
