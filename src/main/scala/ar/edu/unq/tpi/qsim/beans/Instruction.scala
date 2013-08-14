package ar.edu.unq.tpi.qsim.beans
import ar.edu.unq.tpi.qsim.utils._

class Instruction(val codeOperation: String, var name: String, var destiny: ModeAddressing, var origin: ModeAddressing){

def toHexString() : String  =  {
  val codeOperation_hex = Util.binaryToHex(codeOperation)
  val destiny_code = Util.binaryToHex(destiny.code)
  val destiny_value = destiny.getValue
  
  val origin_code = Util.binaryToHex(origin.code)
  val origin_value = origin.getValue
    
  (codeOperation + " " + destiny_code + " " + origin_code + " " + destiny_value + " " + origin_value).replace("  "," ")
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


  val e = ADD(R1,Immediate("0F13"))
  println(e.toString())
  println(e.toHexString())
}