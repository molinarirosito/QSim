package ar.edu.unq.tpi.qsim.model


class Celda (var w16: W16) {
  

  
  override def toString() =  w16.toString
  
  def setW16(word16: W16) = this.w16 = word16
  def getW16():  W16 = this.w16 
  
 
   

}