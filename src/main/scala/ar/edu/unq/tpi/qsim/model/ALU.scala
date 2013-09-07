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

}