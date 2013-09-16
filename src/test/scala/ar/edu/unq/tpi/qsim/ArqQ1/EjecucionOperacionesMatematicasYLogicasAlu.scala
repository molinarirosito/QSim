package ar.edu.unq.tpi.qsim.ArqQ1

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import ar.edu.unq.tpi.qsim.model.ALU
import ar.edu.unq.tpi.qsim.model.W16
import ar.edu.unq.tpi.qsim.utils.Util

class EjecucionOperacionesMatematicasYLogicasAlu extends FlatSpec with Matchers {

  def contexto_ejecucion = new {
    val alu = ALU
    var op1 = new W16("0002")
    var op2 = new W16("0006")

  }

  "Una Alu" should "ejecutar la suma entre dos numeros hexadecimales en el sist CA2 y actualizar todos los flags" in {

    var ctx = contexto_ejecucion
  }

  it should "ejecutar la resta entre dos numeros hexadecimales en el sist CA2 y actualizar todos los flags " in {
    var ctx = contexto_ejecucion
  }

  it should "ejecutar la multiplicacion entre dos numeros hexadecimales en el sist CA2 y actualizar los flags N y Z con C y N = 0 " in {
    var ctx = contexto_ejecucion
  }

  it should "ejecutar la division entre dos numeros hexadecimales en el sist CA2 y actualizar los flags N y Z con C y N = 0 " in {
    var ctx = contexto_ejecucion
  }

  it should "ejecutar la operacion logica AND entre dos numeros hexadecimales en el sist CA2 y actualizar los flags N y Z con C y N = 0 " in {
    var ctx = contexto_ejecucion

  }

  it should "executar la operacion logica OR entre dos numeros hexadecimales en el sist CA2 y actualizar los flags N y Z con C y N = 0 " in {
    var ctx = contexto_ejecucion

  }

  it should "executar la suma entre dos numeros hexadecimales en el sist CA2 " in {
  }
}