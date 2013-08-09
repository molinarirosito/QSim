package ar.edu.unq.tpi.qsim.beans

class Instruction(val codeOperation: Int, var name: String, var destiny: String, var origin: String){

override def toString() =  name + " " + destiny + " " + origin
  
}

case class MUL(dest: String, orig: String) extends Instruction(0000,"MUL",dest,orig){
}

case class ADD(dest: String, orig: String) extends Instruction(0010,"ADD",dest,orig){
}

case class SUB(dest: String, orig: String) extends Instruction(0011,"SUB",dest,orig){
}

case class DIV(dest: String, orig: String) extends Instruction(0111,"MUL",dest,orig){
}

case class MOV(dest: String, orig: String) extends Instruction(0001,"ADD",dest,orig){
}


object Test extends App{
  var v = Integer.parseInt("0F",16)
  print(v)
  val t = MUL("soy operando uno","soy operando dos")
  print(t.toString())
}