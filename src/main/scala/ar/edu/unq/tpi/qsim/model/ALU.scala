package ar.edu.unq.tpi.qsim.model
import ar.edu.unq.tpi.qsim.utils.Util

class ALU {
  
  // para mi la alu deberia de devolver una tupla con los flags modificados de haber realizado cada operacion :D
  
  def execute_add(op1: W16, op2: W16 ) :W16 = {
   op1 + op2
  }
  
  def execute_mul(op1: W16, op2: W16 ) :W16 = {
   op1 * op2
  }
  
  def execute_sub(op1: W16, op2: W16 ) :W16 = {
   op1 - op2
  }
 
  def execute_div(op1: W16, op2: W16 ) :W16 = {
   op1 / op2
  }
  
  def actualizarNegative(resultado: W16) : Int = resultado.toBinary.charAt(0).toInt
  def actualizarZero(resultado: W16): Int = if (resultado.value == 0) {1}  else {0}
  def actualizarCarry(resultado: W16, op1 :W16, op2 : W16) : Int = {
    val bit_significativo_op1 =  op1.toBinary.charAt(0).toInt 
    val bit_significativo_op2 =  op2.toBinary.charAt(0).toInt
    if((bit_significativo_op1 == bit_significativo_op2) && (bit_significativo_op1  == 1))
    { 1 }
    else {0}
  }
  def actualizarBorrow(resultado: W16, op1 :W16, op2 : W16) : Int = {
    val bit_significativo_resultado =  ( resultado.hex).charAt(0).toInt 
    val bit_significativo_op1 =  op1.toBinary.charAt(0).toInt 
    val bit_significativo_op2 =  op2.toBinary.charAt(0).toInt
    if((bit_significativo_op1 == 0) && (bit_significativo_op2  == 1)&& (bit_significativo_resultado  == 1))
    { 1 }
    else {0}
  }

}