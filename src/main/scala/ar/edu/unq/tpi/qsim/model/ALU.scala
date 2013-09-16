package ar.edu.unq.tpi.qsim.model
import ar.edu.unq.tpi.qsim.utils.Util
import scala.collection.mutable.Map

object ALU {

  def execute_operacion_matematica(operacion: (Int, Int) ⇒ Int, op1: W16, op2: W16): Map[String, Any] = {

    val valor = operacion(op1.value, op2.value)

    val resultado_binario = Util.toBinary16BOverflow(valor)
    println("resultado " + resultado_binario)
    println("valor" + valor)
    val n = actualizarNegative(valor)
    val z = actualizarZero(valor)
    // val c = 0
    val c = actualizarCarryBorrow(resultado_binario)
    val v = 0

    var resultado = new W16(Util.fromBinaryToHex4(resultado_binario))
    // resultado.signo = signo
    Map(("resultado", resultado), ("n", n), ("z", z), ("c", c), ("v", v))

  }

  def execute_add(op1: W16, op2: W16): Map[String, Any] = {
    var resultados = execute_operacion_matematica(_ + _, op1, op2)
    val v = verificarCondicionOverflowSuma(resultados("resultado").asInstanceOf[W16], op1, op2)
    resultados("v") = v.asInstanceOf[Any]
    resultados
  }

  def execute_sub(op1: W16, op2: W16): Map[String, Any] = {
    var resultados = execute_operacion_matematica(_ - _, op1, op2)
    val v = verificarCondicionOverflowResta(resultados("resultado").asInstanceOf[W16], op1, op2)
    resultados("v") = v.asInstanceOf[Any]
    resultados

  }

  def execute_mul(op1: W16, op2: W16): Map[String, Any] = {
    execute_operacion_matematica(_ * _, op1, op2)
    // carry 0
  }

  def execute_div(op1: W16, op2: W16): Map[String, Any] = {
    execute_operacion_matematica(_ / _, op1, op2)
    //   carry 0
  }

  def execute_cmp(op1: W16, op2: W16): Map[String, Any] = {
    var resultados = execute_operacion_matematica(_-_, op1, op2)
    val v = verificarCondicionOverflowResta(resultados("resultado").asInstanceOf[W16], op1, op2)
    resultados("v") = v.asInstanceOf[Any]
    resultados
  }
  def actualizarNegative(resultado: Int): Int = resultado match {
    case r if (r < 0) ⇒ 1
    case _ ⇒ 0
  }

  def actualizarZero(resultado: Int): Int = resultado match {
    case r if (r == 0) ⇒ 1
    case _ ⇒ 0
  }
  
  def actualizarCarryBorrow(resultado_binario: String): Int = Integer.parseInt(resultado_binario.charAt(0).toString)

  def obtenerBitsParaAnalizarOverflow(resultado_binario: W16, op1: W16, op2: W16): (Int, Int, Int) = {
    val bit_significativo_resultado = Integer.parseInt(resultado_binario.toBinary.charAt(0).toString)
    val bit_significativo_op1 = Integer.parseInt(op1.toBinary.charAt(0).toString)
    val bit_significativo_op2 = Integer.parseInt(op2.toBinary.charAt(0).toString)
    (bit_significativo_op1, bit_significativo_op2, bit_significativo_resultado)
  }

  def verificarCondicionOverflowSuma(resultado_binario: W16, op1: W16, op2: W16): Int = {
    var bits = obtenerBitsParaAnalizarOverflow(resultado_binario, op1, op2)
    if ((bits._1 == bits._2) && (bits._2 != bits._3)) { 1 } else { 0 }
  }

  def verificarCondicionOverflowResta(resultado_binario: W16, op1: W16, op2: W16): Int = {
    var bits = obtenerBitsParaAnalizarOverflow(resultado_binario, op1, op2)
    if ((bits._1 != bits._2) && (bits._2 == bits._3)) { 1 } else { 0 }
  }
}