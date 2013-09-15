package ar.edu.unq.tpi.qsim.model
import ar.edu.unq.tpi.qsim.utils._

class Salto(var salto: Int) {
	override def toString() =  value.toString
	def toBinary: String = Util.toBinary8B(salto)
	def value : Int = salto
	
	def this(binary_chain: String) {
    this(Util.binaryToInteger(binary_chain));

  }
}