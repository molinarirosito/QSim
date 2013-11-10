package ar.edu.unq.tpi.qsim.model
import org.uqbar.commons.utils.Observable

object State extends Enumeration {
  type Type = Value
  val NONE, PROGRAM, FECH_DECODE, STORE, EXECUTED = Value
}

@Observable
class Celda (var value: W16) {
  var state = State.NONE
  
  override def toString() =  value.toString
  
  def setW16(word16: W16) = this.value = word16
  def getW16():  W16 = this.value 
 
   

}