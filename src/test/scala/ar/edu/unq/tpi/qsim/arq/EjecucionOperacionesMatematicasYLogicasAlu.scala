package ar.edu.unq.tpi.qsim.arq

/**
* Copyright 2014 Tatiana Molinari.
* Copyright 2014 Susana Rosito
*
* This program is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program. If not, see <http://www.gnu.org/licenses/>.
*
*/

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
    var op3 = new W16("0001")
    var op4 = new W16("FFE0")
    var opunoresta = new W16("1000")
    var opdosresta = new W16("F000")
  }
  

  def resultados_esperados = new {
    var resultado_suma = new W16("0008")
    var resultado_resta = new W16("0004")
    var resultado_mult = new W16("000C")
    var resultado_div = new W16("0003")
    var resultado_and = new W16("0002")
    var resultado_or = new W16("0006")
    var resultado_sub = new W16("2000")
    var resultado_suma_ca2 = new W16("FFE1")
  }
  def resultados_flags = new {
    var v = (0, 1)
    var c = (0, 1)
    var z = (0, 1)
    var n = (0, 1)
  }

  "Una Alu" should "ejecutar la suma entre dos numeros hexadecimales en el sist CA2 y actualizar todos los flags" in {
    var ctx = contexto_ejecucion
    var mapa_resultados = ctx.alu.execute_add(ctx.op1, ctx.op2)
    var set_resultados = resultados_esperados
    var set_resultados_flags = resultados_flags

    // VERIFICA EL RESULTADO
    assert(mapa_resultados("resultado").asInstanceOf[W16].equals(set_resultados.resultado_suma))

    // VERIFICA LOS FLAGS
    assert(mapa_resultados("v") === set_resultados_flags.v._1)
    assert(mapa_resultados("c") === set_resultados_flags.c._1)
    assert(mapa_resultados("z") === set_resultados_flags.z._1)
    assert(mapa_resultados("n") === set_resultados_flags.n._1)
  }
  
  "Una Alu" should "ejecutar la sumaca2 entre dos numeros en CA2 y actualizar los flags" in {
    var ctx = contexto_ejecucion
    var mapa_resultados = ctx.alu.execute_add(ctx.op3, ctx.op4)
    print(mapa_resultados)
    var set_resultados = resultados_esperados
    var set_resultados_flags = resultados_flags

    // VERIFICA EL RESULTADO
    assert(mapa_resultados("resultado").asInstanceOf[W16].equals(set_resultados.resultado_suma_ca2))

    // VERIFICA LOS FLAGS
    assert(mapa_resultados("v") === set_resultados_flags.v._1)
    assert(mapa_resultados("c") === set_resultados_flags.c._1)
    assert(mapa_resultados("z") === set_resultados_flags.z._1)
    assert(mapa_resultados("n") === set_resultados_flags.n._2)
  }
  
  it should "ejecutar la resta entre dos numeros hexadecimales en el sist CA2 y actualizar todos los flags " in {
    var ctx = contexto_ejecucion
    var mapa_resultados = ctx.alu.execute_sub(ctx.op2, ctx.op1)
    var set_resultados = resultados_esperados
    var set_resultados_flags = resultados_flags

    // VERIFICA EL RESULTADO
    assert(mapa_resultados("resultado").asInstanceOf[W16].equals(set_resultados.resultado_resta))

    // VERIFICA LOS FLAGS
    assert(mapa_resultados("v") === set_resultados_flags.v._1)
    assert(mapa_resultados("c") === set_resultados_flags.c._1)
    assert(mapa_resultados("z") === set_resultados_flags.z._1)
    assert(mapa_resultados("n") === set_resultados_flags.n._1)

  }

  it should "ejecutar la multiplicacion entre dos numeros hexadecimales en el sist CA2 y actualizar los flags N y Z con C y N = 0 " in {

    var ctx = contexto_ejecucion
    var mapa_resultados = ctx.alu.execute_mul(ctx.op2, ctx.op1)
    var set_resultados = resultados_esperados
    var set_resultados_flags = resultados_flags

    // VERIFICA EL RESULTADO
    assert(mapa_resultados("resultado").asInstanceOf[W16].equals(set_resultados.resultado_mult))

    // VERIFICA LOS FLAGS
    assert(mapa_resultados("v") === set_resultados_flags.v._1)
    assert(mapa_resultados("c") === set_resultados_flags.c._1)
    assert(mapa_resultados("z") === set_resultados_flags.z._1)
    assert(mapa_resultados("n") === set_resultados_flags.n._1)

  }

  it should "ejecutar la division entre dos numeros hexadecimales en el sist CA2 y actualizar los flags N y Z con C y N = 0 " in {
    var ctx = contexto_ejecucion
    var mapa_resultados = ctx.alu.execute_div(ctx.op2, ctx.op1)
    var set_resultados = resultados_esperados
    var set_resultados_flags = resultados_flags

    // VERIFICA EL RESULTADO
    assert(mapa_resultados("resultado").asInstanceOf[W16].equals(set_resultados.resultado_div))

    // VERIFICA LOS FLAGS
    assert(mapa_resultados("v") === set_resultados_flags.v._1)
    assert(mapa_resultados("c") === set_resultados_flags.c._1)
    assert(mapa_resultados("z") === set_resultados_flags.z._1)
    assert(mapa_resultados("n") === set_resultados_flags.n._1)

  }

  it should "ejecutar la comparacion entre dos numeros hexadecimales en el sist CA2 y actualizar los flags N y Z con C y N = 0 " in {
    var ctx = contexto_ejecucion
    var mapa_resultados = ctx.alu.execute_cmp(ctx.op2, ctx.op1)
    var set_resultados = resultados_esperados
    var set_resultados_flags = resultados_flags

    // VERIFICA EL RESULTADO
    assert(mapa_resultados("resultado").asInstanceOf[W16].equals(set_resultados.resultado_resta))

    // VERIFICA LOS FLAGS
    assert(mapa_resultados("v") === set_resultados_flags.v._1)
    assert(mapa_resultados("c") === set_resultados_flags.c._1)
    assert(mapa_resultados("z") === set_resultados_flags.z._1)
    assert(mapa_resultados("n") === set_resultados_flags.n._1)

  }

  it should "ejecutar la operacion logica AND entre dos numeros hexadecimales en el sist CA2 y actualizar los flags N y Z con C y N = 0 " in {
    var ctx = contexto_ejecucion
    var mapa_resultados = ctx.alu.AND(ctx.op1, ctx.op2)
    var set_resultados = resultados_esperados
    var set_resultados_flags = resultados_flags

    // VERIFICA EL RESULTADO
    assert(mapa_resultados("resultado").asInstanceOf[W16].equals(set_resultados.resultado_and))

    // VERIFICA LOS FLAGS
    assert(mapa_resultados("v") === set_resultados_flags.v._1)
    assert(mapa_resultados("c") === set_resultados_flags.c._1)
    assert(mapa_resultados("z") === set_resultados_flags.z._1)
    assert(mapa_resultados("n") === set_resultados_flags.n._1)

  }

  it should "executar la operacion logica OR entre dos numeros hexadecimales en el sist CA2 y actualizar los flags N y Z con C y N = 0 " in {
    var ctx = contexto_ejecucion
    var mapa_resultados = ctx.alu.OR(ctx.op2, ctx.op1)
    var set_resultados = resultados_esperados
    var set_resultados_flags = resultados_flags

    // VERIFICA EL RESULTADO
    assert(mapa_resultados("resultado").asInstanceOf[W16].equals(set_resultados.resultado_or))

    // VERIFICA LOS FLAGS
    assert(mapa_resultados("v") === set_resultados_flags.v._1)
    assert(mapa_resultados("c") === set_resultados_flags.c._1)
    assert(mapa_resultados("z") === set_resultados_flags.z._1)
    assert(mapa_resultados("n") === set_resultados_flags.n._1)

  }
  it should "executar la operacion logica SUB entre dos numeros hexadecimales en el sist CA2 y actualizar los flags dejando el carry en 1 " in {
    var ctx = contexto_ejecucion
    var mapa_resultados = ctx.alu.execute_sub(ctx.opunoresta, ctx.opdosresta)
    var set_resultados = resultados_esperados
    var set_resultados_flags = resultados_flags
    println(mapa_resultados("resultado").asInstanceOf[W16].toString)
    // VERIFICA EL RESULTADO
    assert(mapa_resultados("resultado").asInstanceOf[W16].equals(set_resultados.resultado_sub))

    // VERIFICA LOS FLAGS
    assert(mapa_resultados("v") === set_resultados_flags.v._1)
    assert(mapa_resultados("c") === set_resultados_flags.c._2)
    assert(mapa_resultados("z") === set_resultados_flags.z._1)
    assert(mapa_resultados("n") === set_resultados_flags.n._1)

  }
}
