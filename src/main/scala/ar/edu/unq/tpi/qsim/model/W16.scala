package ar.edu.unq.tpi.qsim.model

import ar.edu.unq.tpi.qsim.utils.Util


class W16(var hex: String) {
  
  override def toString() =  this.hex
  def equals(w16:W16): Boolean =  this.value == w16.value
  def value : Int = Util.toInteger(this.hex)
  def toBinary: String = Util.hexToBinary(this.hex)
  def ++ = hex = Util.toHex4(this.value + 1)
  def ++(salto:Int) = hex= Util.toHex4(this.value + salto)
  def -- = hex= Util.toHex4(this.value -1)
  
  def :=(w16 : String) = hex = w16
  
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
  def operacion_matematica(op : (Int,Int)=>Int, op1 : Int, op2 : Int)
  {
    println(op(op1, op2))
    
  }

}

object prueba extends App() {

  var w = new W16("0001")
  var w2 = new W16("0010")
  
 
  println(w2-w2)
  println(w2-w)
  w.operacion_matematica(_+_, 1, 2)
  
}
  

