package ar.edu.unq.tpi.qsim.model
import ar.edu.unq.tpi.qsim.utils.Util
import scala.collection.mutable.Map

object ALU {

  def execute_operacion_matematica(operacion: (Int, Int) ⇒ Int, op1: W16, op2: W16): Map[String, Any] = {

    val valor = operacion(op1.value, op2.value)
    val resultado_binario = Util.toBinary16BOverflow(valor)
    println("resultado " + valor )
    println("resultado " + resultado_binario )
    val flags = takeFlags(valor)
    
    var resultado = new W16(Util.fromBinaryToHex4(resultado_binario))
    Map(("resultado", resultado), ("n", flags._1), ("z",flags._2), ("c",0), ("v",0))

  }
  def takeFlags(valor: Int): (Int, Int) = (actualizarNegative(valor), actualizarZero(valor))

  def takeFlagsSum(resultado_binario: W16, op1: W16, op2: W16): (Int, Int) = {
    val c = actualizarCarryBorrow(resultado_binario)
    val v = verificarCondicionOverflowSuma(resultado_binario, op1, op2)
    (c, v)
  }

  def takeFlagsRest(resultado_binario: W16, op1: W16, op2: W16): (Int, Int) = {
    val c = actualizarCarryBorrow(resultado_binario)
    val v = verificarCondicionOverflowResta(resultado_binario, op1, op2)
    (c, v)
  }

  def execute_add(op1: W16, op2: W16): Map[String, Any] = {
    var resultados = execute_operacion_matematica(_ + _, op1, op2)
    val carryOverflow = takeFlagsSum(resultados("resultado").asInstanceOf[W16], op1, op2)
    resultados("c") = carryOverflow._1.asInstanceOf[Any]
    resultados("v") = carryOverflow._2.asInstanceOf[Any]

    resultados
  }

  def execute_sub(op1: W16, op2: W16): Map[String, Any] = {
    var resultados = execute_operacion_matematica(_ - _, op1, op2)
    val carryOverflow = takeFlagsRest(resultados("resultado").asInstanceOf[W16], op1, op2)
    resultados("c") = carryOverflow._1.asInstanceOf[Any]
    resultados("v") = carryOverflow._2.asInstanceOf[Any]
    resultados

  }

  def execute_mul(op1: W16, op2: W16): Map[String, Any] = {
    execute_operacion_mul(_ * _, op1, op2)
  }

  def execute_div(op1: W16, op2: W16): Map[String, Any] = {
    execute_operacion_matematica(_ / _, op1, op2)
  }

  def execute_cmp(op1: W16, op2: W16): Map[String, Any] = {
    var resultados = execute_operacion_matematica(_ - _, op1, op2)
    val carryOverflow = takeFlagsRest(resultados("resultado").asInstanceOf[W16], op1, op2)
    resultados("c") = carryOverflow._1.asInstanceOf[Any]
    resultados("v") = carryOverflow._2.asInstanceOf[Any]
    resultados
  }

  def execute_operacion_mul(operacion: (Int, Int) ⇒ Int, op1: W16, op2: W16): Map[String, Any] = {

    val valor = operacion(op1.value, op2.value)
    val resultado_binario = Util.toBinary32B(valor)
    val flags = takeFlags(valor)
    var resultadoMenosSignificativo = new W16(Util.fromBinaryToHex4(resultado_binario._2))
    var resultadoMasSignificativo = new W16(Util.fromBinaryToHex4(resultado_binario._1))

    Map(("R7", resultadoMasSignificativo), ("resultado", resultadoMenosSignificativo), ("n", flags._1), ("z", flags._2), ("c", 0), ("v", 0))

  }

  def actualizarNegative(resultado: Int): Int = resultado match {
    case r if (r < 0) ⇒ 1
    case _ ⇒ 0
  }

  def actualizarZero(resultado: Int): Int = resultado match {
    case r if (r == 0) ⇒ 1
    case _ ⇒ 0
  }

  def actualizarCarryBorrow(resultado_binario: W16): Int = Integer.parseInt(resultado_binario.toBinary.charAt(0).toString)

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