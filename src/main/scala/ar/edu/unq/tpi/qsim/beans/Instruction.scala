package ar.edu.unq.tpi.qsim.beans

class Instruction(){

  var operationCode: Int =_
  var name: String =_
  var op1: String =_
  var op2: String =_
  
  

override def toString() = 
     name + " " + op1 + " " + op2
  
}

case class MUL(operand1: String, operand2: String) extends Instruction
{

  operationCode = 0000
  name = "MUL"
  op1 = operand1
  op2 = operand2
  
}
object Test extends App{
  var v = Integer.parseInt("0F",16)
  print(v)
  val t = MUL("soy operando uno","soy operando dos")
  print(t.toString())
}