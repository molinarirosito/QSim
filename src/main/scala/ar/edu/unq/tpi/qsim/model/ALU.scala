package ar.edu.unq.tpi.qsim.model
import ar.edu.unq.tpi.qsim.utils.Util

object ALU {
  
  def execute_operacion_matematica(operacion:(Int,Int)=>Int,op1: W16, op2: W16 ) : Map[String,Any] = {
   
    val valor = operacion(op1.value,op2.value)
    val resultado_binario = Util.toBinary16BOverflow(valor)
    val n = actualizarNegative(valor)
    val z = actualizarZero(valor)
    val c = actualizarCarryBorrow(resultado_binario, valor)
    val v = actualizarOverflow(resultado_binario, op1, op2)
    
    val resultado = new W16(Util.toHex4(valor.abs))
    Map(("resultado",resultado),("n",n),("z",z),("c",c),("v",v))
    
  }
  
  def execute_add(op1: W16, op2: W16 ) : Map[String,Any] = {
   execute_operacion_matematica(_+_, op1, op2)

  }
  
   def execute_sub(op1: W16, op2: W16 ) : Map[String,Any] = {
   execute_operacion_matematica(_-_, op1, op2)

  }
   
   def execute_mul(op1: W16, op2: W16 ) : Map[String,Any] = {
   execute_operacion_matematica(_*_, op1, op2)

  }
    
   def execute_div(op1: W16, op2: W16 ) : Map[String,Any] = {
   execute_operacion_matematica(_/_, op1, op2)

  }
  
  def actualizarNegative(resultado: Int) : Int = if (resultado < 0) {1}  else {0}
  def actualizarZero(resultado: Int): Int = if (resultado == 0) {1}  else {0}
  def actualizarCarryBorrow(resultado_binario: String, resultado: Int) : Int = if (resultado_binario.size>16 | resultado<0 ) {1}  else {0}
  def actualizarOverflow(resultado_binario: String, op1 :W16, op2 : W16) : Int = {
    val bit_significativo_resultado =  resultado_binario.charAt(0).toInt 
    val bit_significativo_op1 =  op1.toBinary.charAt(0).toInt 
    val bit_significativo_op2 =  op2.toBinary.charAt(0).toInt
    if((bit_significativo_op1 == bit_significativo_op2) && (bit_significativo_op2 != bit_significativo_resultado) |
       (bit_significativo_op1 != bit_significativo_op2) && (bit_significativo_op2 == bit_significativo_resultado))
    { 1 }
    else {0}
  }

}