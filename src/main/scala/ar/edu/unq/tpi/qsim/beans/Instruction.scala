package ar.edu.unq.tpi.qsim.beans

class Instruction(var operationCode: Int, var name: String, var op1: String, var op2: String){

override def toString() =  name + " " + op1 + " " + op2
  
}

case class MUL(operand1: String, operand2: String) extends Instruction(0000,"MUL",operand1,operand2)
{

}

object Test extends App{
  var v = Integer.parseInt("0F",16)
  print(v)
  val t = MUL("soy operando uno","soy operando dos")
  print(t.toString())
}