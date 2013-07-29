package ar.edu.unq.tpi.qsim.beans

class Instruction(var operationCode: Int, var name: String, var destino: String, var origen: String){

override def toString() =  name + " " + destino + " " + origen
  
}

case class MUL(destiny: String, origin: String) extends Instruction(0000,"MUL",destiny,origin)
{

}

object Test extends App{
  var v = Integer.parseInt("0F",16)
  print(v)
  val t = MUL("soy operando uno","soy operando dos")
  print(t.toString())
}