package ar.edu.unq.tpi.qsim.model
import org.uqbar.commons.utils.Observable

@Observable
class Celda (var value: W16) {
  override def toString() =  value.toString
  
  def setW16(word16: W16) = this.value = word16
  def getW16():  W16 = this.value 
 
   

}