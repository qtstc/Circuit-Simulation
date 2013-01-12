package quinemccluskey

import scala.collection.mutable.ListBuffer
import quinemccluskey.Term._

class Formula (newTerms:ListBuffer[Term]){
  var terms:ListBuffer[Term] = newTerms
  var originalTerms = newTerms.toList
  
  def reduceToPrimeImplicants() 
  {
		val numVars = terms(0).length;
		var table = Array.ofDim[ListBuffer[Term]](numVars+1, numVars+1)
	    for (dontKnows <- 0 to numVars) 
	    for (ones <- 0 to numVars) 
			table(dontKnows)(ones) = ListBuffer()
				
		for (t<-terms) {
			val dontCares = countValue(t,DONT_CARE)
			val ones = countValue(t,TRUE)
			table(dontCares)(ones).append(t)
		}
		for (dontKnows <- 0 to numVars - 1) 
			for (ones <- 0 to numVars - 1) {
				var left = table(dontKnows)(ones)
				var right = table(dontKnows)(ones + 1)
				var out = table(dontKnows + 1)(ones)
				for (leftOne <- left) {
					for (rightOne <-right) {
						val combined = combine(leftOne,rightOne)
						if (combined != null) {
							if (!out.contains(combined)) {
								out.append(combined)
							}
							terms -= leftOne;
							terms -= rightOne
							if (!terms.contains(combined))
								terms.append(combined)
						}
					}
				}
			}
	}
  
   def reducePrimeImplicantsToSubset() 
   {
		val numPrimeImplicants = terms.length
		val numOriginalTerms = originalTerms.length
		var table = Array.ofDim[Boolean](numPrimeImplicants, numOriginalTerms)
		for (impl <- 0 until numPrimeImplicants) 
			for (term <- 0 until numOriginalTerms) 
				table(impl)(term) = terms(impl).implies(originalTerms(term))
		
		val newTermList = ListBuffer[Term]()
		
		var done = false
		var impl = 0
		while (!done) {
			impl = extractEssentialImplicant(table)
			if (impl != -1) 
				newTermList.append((terms(impl))) 
			else {
				impl = extractLargestImplicant(table)
				if (impl != -1) {
					newTermList.append(terms(impl))
				} else {
					done = true;
				}
			}
		}
		terms = newTermList;
		originalTerms = null;
	}
   
   def extractEssentialImplicant( table:Array[Array[Boolean]]):Int =
   {
		for (term <- 0 until table(0).length) 
		{
			var lastImplFound = -1;
			var breaked = false
			for (impl <- 0 until table.length) 
			{
			  if(!breaked)
			  {
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

	def extractImplicant(table:Array[Array[Boolean]],impl:Int) 
	{
		for (term <- 0 until table(0).length) {
			if (table(impl)(term)) {
				for (impl2 <- 0 until table.length) {
					table(impl2)(term) = false
				}
			}
		}
	}

	def extractLargestImplicant(table:Array[Array[Boolean]]):Int  =
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
   
	
	override def toString():String = 
	{
	    var result = "";
		result += terms.length + " terms, " + terms(0).length + " variables\n";
		for (e<-terms) {
			result += e + "\n";
		}
		return result;
	}
  
}