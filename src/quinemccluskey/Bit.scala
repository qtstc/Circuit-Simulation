package quinemccluskey

object Bit
{ 
  def apply(c:Char) = c match
  {
    case TRUE.charRepresentation =>
      TRUE
    case FALSE.charRepresentation =>
      FALSE
    case DONT_CARE.charRepresentation =>
      DONT_CARE
    case _ =>
      throw new IllegalArgumentException("A bit can either be '1','0' or 'X'.")
  }
}
abstract class Bit
{
  val charRepresentation:Char
  override def toString = charRepresentation.toString()
}
object TRUE extends Bit
{
  val charRepresentation = '1'
}
object FALSE extends Bit
{
  val charRepresentation = '0'
}
object DONT_CARE extends Bit
{
  val charRepresentation = 'X'
}