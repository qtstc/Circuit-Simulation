package quinemccluskey

import quinemccluskey._

object Test{
  def main(args: Array[String]) {
    
    val t1 = new Term(TRUE,FALSE,DONT_CARE)
    val t2 = new Term(TRUE,FALSE,DONT_CARE)
    println(t1 == t2)
  }
}

