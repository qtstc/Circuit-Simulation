package quinemccluskey

object Term
{
  /**
   * Combines two terms.
   * Two terms can be combined if:
   * 1)They only differ at one bit
   * 2)For both terms, that bit cannot be DONT_CARE
   * 
   * If two terms can not be combined, null will be returned.
   * Else a new term will be created and returned.
   * The bit where the original two terms differs has 
   * the value DONT_CARE in the new term.
   */
  def combine(t1:Term, t2:Term) : Term =
  {
    //First check the length
    if(t1.length != t2.length)
      throw new IllegalArgumentException("The two terms to be combined must have the same length.")
    
    //Compare each bit of the two terms
    var diffNum = -1
    for(i<-0 until t1.length)
    {
      val b1 = t1.bits(i)
      val b2 = t2.bits(i)
      if(b1 != b2)
      {
        if(b1 == DONT_CARE || b2 == DONT_CARE)
          return null
        if(diffNum != -1)
          return null
        diffNum = i
      }
    }
    if(diffNum == -1)//If t1 and t2 are the same
      null
    else
    {
      val t3 = new Term(t1)
      t3(diffNum) =DONT_CARE
      t3
    }
  }
  
  /**
   * Count the number of certain bit in a term.
   */
  def countValue(t:Term,value:Bit) = countValueHelper(t.bits.toList,value)
  
  /**
   * Helper method for countValue().Required because we cannot
   * do head::tail pattern matching on Array.
   */
  private def countValueHelper(l:List[Bit], value:Bit):Int = l match
  {
    case List() => 0
    case head::tail =>
      val current = if(head == value) 1 else 0
      current + countValueHelper(tail,value)
  }
}

class Term(termBits:Array[Bit]){
  
  def this(repeatedBits:Bit*){this(repeatedBits.toArray)}
  def this(bitString:String){this(bitString.toArray.map(Bit.apply))}
  def this(t:Term){this(t.bits.clone)}
  
  val bits = termBits
  val length = bits.length
  
  override def toString = 
  {
    var text = ""
    for(b<-bits)
      text += b
    text
  }
  
  def update(i:Int,b:Bit):Term = 
  {
    bits(i) = b
    this
  }
  
  def ==(t:Term):Boolean = 
  {
    if(length != t.length)
      return false
    for(i <- 0 until length)
      if(bits(i) != t.bits(i))
        return false
    return true
  }
}