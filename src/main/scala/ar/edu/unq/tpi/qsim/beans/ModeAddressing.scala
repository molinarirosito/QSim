package ar.edu.unq.tpi.qsim.beans

class ModeAddressing(var value:String, var amountCellOccupies : Int) {
}

case class Register extends ModeAddressing("",6){
  
}

case class immediate extends ModeAddressing("FFFF",6){
  
}