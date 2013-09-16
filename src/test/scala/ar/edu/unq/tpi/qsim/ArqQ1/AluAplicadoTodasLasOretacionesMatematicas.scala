package ar.edu.unq.tpi.qsim.ArqQ1

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import ar.edu.unq.tpi.qsim.model.ALU
import ar.edu.unq.tpi.qsim.model.W16
import ar.edu.unq.tpi.qsim.utils.Util

class AluAplicadoTodasLasOretacionesMatematicas extends FlatSpec with Matchers {



  "Una Alu" should "executar la suma entre dos numeros hexadecimales en el sist CA2 " in {
	  var alu = ALU
	  var utl = Util
	  //var lala = new W16("FFFF").signo
	  //lala = -(lala)
	 //println(lala)
	 println(alu.execute_sub(new W16("F000"), new W16("0001")))
	  //println(Util.fromBinaryToHex4("11111111100001101"))
  }


}
