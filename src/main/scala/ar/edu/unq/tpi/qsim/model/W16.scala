package ar.edu.unq.tpi.qsim.model

import ar.edu.unq.tpi.qsim.utils.Util


class W16(var hex: String) {
  
  override def toString() =  this.hex
  def value : Int = Util.toInteger(this.hex)
  def toBinary: String = Util.hexToBinary(this.hex)
  
  def +(w16 : W16) :W16 = {
    
   val result_value = Util.toHex(this.value+w16.value)
   new W16(result_value)
  }
  def -(w16 : W16) :W16 = {
    
   val result_value = Util.toHex(this.value-w16.value)
   new W16(result_value)
  }
  
  def *(w16 : W16) :W16 = {
    
   val result_value = Util.toHex(this.value*w16.value)
   new W16(result_value)
  }
  
  def /(w16 : W16) :W16 = {
    
   val result_value = Util.toHex(this.value/w16.value)
   new W16(result_value)
  }
  

}
  

