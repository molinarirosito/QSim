package ar.edu.unq.tpi.qsim.beans
import ar.edu.unq.tpi.qsim.utils.Util

class ALU {
  
  def execute(add : ADD) :String = {
    val value_destiny = add.destiny.value_saved
    val value_origin = add.origin.value_saved
    val result = Util.toInteger(value_destiny) + Util.toInteger(value_origin)
    Util.toHex(result)
    
  }
  
  def execute(mul : MUL) :String = {
    val value_destiny = mul.destiny.value_saved
    val value_origin = mul.origin.value_saved
    val result = Util.toInteger(value_destiny)*Util.toInteger(value_origin)
    Util.toHex(result)
    
  }
  
  def execute(sub : SUB) :String = {
    val value_destiny = sub.destiny.value_saved
    val value_origin = sub.origin.value_saved
    val result = Util.toInteger(value_destiny) - Util.toInteger(value_origin)
    Util.toHex(result)
    
  }
  
  def execute(div : DIV) :String = {
    val value_destiny = div.destiny.value_saved
    val value_origin = div.origin.value_saved
    val result = Util.toInteger(value_destiny)/Util.toInteger(value_origin)
    Util.toHex(result)
    
  }

}