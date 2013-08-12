package ar.edu.unq.tpi.qsim.beans

class Instruction(val codeOperation: String, var name: String, var destiny: ModeAddressing, var origin: ModeAddressing){

def toBinaryString() : String  =  {
//  val destiny_code = destiny.toBinaryString().split(",")(0)
 // val destiny_value = destiny.toBinaryString().split(",")(1)
  
  //val origin_code = origin.toBinaryString().split(",")(0)
  //val origin_value = origin.toBinaryString().split(",")(1)
    
  //codeOperation + " " + destiny_code + " " + origin_code + " " + destiny_value + " " + origin_value
		"NO ME ROMPAS LAS PELOTAS :)"
}

def sizeOfCells() : Int = destiny.bits() + origin.bits() + 4

 override def toString() =  name + " " + destiny.toString() + " " + origin.toString() 
  
}

case class MUL(dest: ModeAddressing, orig: ModeAddressing) extends Instruction("0000","MUL",dest,orig){

}

case class ADD(dest: ModeAddressing, orig: ModeAddressing) extends Instruction("0010","ADD",dest,orig){
}

case class SUB(dest: ModeAddressing, orig: ModeAddressing) extends Instruction("0011","SUB",dest,orig){
}

case class DIV(dest: ModeAddressing, orig: ModeAddressing) extends Instruction("0111","DIV",dest,orig){
}

case class MOV(dest: ModeAddressing, orig: ModeAddressing) extends Instruction("0001","MOV",dest,orig){
}


object Testud extends App{


  val e = ADD(R1,Immediate("0013"))
  println(e.toString())
  println(e.toBinaryString())
}