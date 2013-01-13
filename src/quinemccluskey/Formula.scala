package quinemccluskey

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Set
import quinemccluskey.Term._

object Formula
{
  private def reduceToPrimeImplicants(terms:List[Term]) = 
  {
    var primeImplicants = terms.toSet
    //First get the length of each term in the original formula
    //This is as the dimensions for the table.
    val numVars = terms(0).length;
    //Define and initialize the table used to group terms.
    var table = Array.ofDim[Set[Term]](numVars + 1, numVars + 1)
    for (dontKnows <- 0 to numVars)
      for (ones <- 0 to numVars)
        table(dontKnows)(ones) = Set()

    //Populate the table with the original terms
    //The cell where each term is put is determined by
    //the number of ones and don't care bits it has.
    for (t <- terms) {
      val dontCares = countValue(t, DONT_CARE)
      val ones = countValue(t, TRUE)
      table(dontCares)(ones) += t
    }
    
    //Scan the table from top to bottom
    for (dontKnows <- 0 to numVars - 1)
      for (ones <- 0 to numVars - 1) {
        var left = table(dontKnows)(ones)
        var right = table(dontKnows)(ones + 1)
        var out = table(dontKnows + 1)(ones)
        //If terms in adjacent cells can be combined,
        //remove the original two terms from the Set of prime implicant
        //and add the result to that list
        for (leftOne <- left) {
          for (rightOne <- right) {
            val combined = combine(leftOne, rightOne)
            if (combined != null) {
              out += combined
              primeImplicants -= leftOne
              primeImplicants -= rightOne
              primeImplicants += combined
            }
          }
        }
      }
    primeImplicants
  }
  
  private  def reducePrimeImplicantsToSubset(terms:List[Term],primeImplicants:collection.immutable.Set[Term]) = 
  {
    val primeImplicantsList = primeImplicants.toList
    //Create a table of booleans to track whether a prime implicant implies an original term
    var table = Array.ofDim[Boolean](primeImplicantsList.length, terms.length)
    for (impl <- 0 until primeImplicantsList.length)
      for (term <- 0 until terms.length)
        table(impl)(term) = primeImplicantsList(impl).implies(terms(term))

    var newTerms = Set[Term]()

    var done = false
    var impl = 0
    
    while (!done) {
      impl = extractEssentialImplicant(table)
      if (impl != -1)
      {
        newTerms += primeImplicantsList(impl)
      }
      else {
        impl = extractLargestImplicant(table)
        if (impl != -1) {
          newTerms += primeImplicantsList(impl)
        } else {
          done = true;
        }
      }
    }
    newTerms
  }
  
    private def extractEssentialImplicant(table: Array[Array[Boolean]]): Int =
    {
      for (term <- 0 until table(0).length) {
        var lastImplFound = -1;
        var breaked = false
        for (impl <- 0 until table.length) {
          if (!breaked) {
            if (table(impl)(term)) {
              if (lastImplFound == -1) {
                lastImplFound = impl;
              } else {
                // This term has multiple implications
                lastImplFound = -1;
                breaked = true;
              }
            }
          }
        }
        if (lastImplFound != -1) {
          extractImplicant(table, lastImplFound);
          return lastImplFound;
        }
      }
      return -1;
    }

  private def extractImplicant(table: Array[Array[Boolean]], impl: Int) {
    for (term <- 0 until table(0).length) {
      if (table(impl)(term)) {
        for (impl2 <- 0 until table.length) {
          table(impl2)(term) = false
        }
      }
    }
  }

  private def extractLargestImplicant(table: Array[Array[Boolean]]): Int =
    {
      var maxNumTerms = 0
      var maxNumTermsImpl = -1
      for (impl <- 0 until table.length) {
        var numTerms = 0
        for (term <- 0 until table(0).length)
          if (table(impl)(term))
            numTerms += 1
        if (numTerms > maxNumTerms) {
          maxNumTerms = numTerms
          maxNumTermsImpl = impl
        }
      }
      if (maxNumTermsImpl != -1) {
        extractImplicant(table, maxNumTermsImpl)
        return maxNumTermsImpl
      }
      return -1
    }
  def simplify(f:Formula):Formula =
  {
    val primeImp = reduceToPrimeImplicants(f.terms)

    new Formula(reducePrimeImplicantsToSubset(f.terms, primeImp).toList)
  }

}
class Formula(newTerms: List[Term]) {
  
  val terms: List[Term] = newTerms

  override def toString(): String =
  {
      terms.toString
  }

}