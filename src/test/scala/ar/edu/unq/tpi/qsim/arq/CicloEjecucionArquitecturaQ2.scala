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
import ar.edu.unq.tpi.qsim.model._
import ar.edu.unq.tpi.qsim.parser._
import ar.edu.unq.tpi.qsim.utils._
import scala.collection.mutable.Map
import ar.edu.unq.tpi.qsim.exeptions.SyntaxErrorException

class CicloEjecucionArquitecturaQ2 extends FlatSpec with Matchers {

  def parsers_resultados = new {
    var path = PathTest()
    var programa_valido = path.getContenido("Q2", 1)
    var programa_invalido = path.getContenido("Q2", 2)
    var parser = Parser
    var resultadoQ2 = parser.ensamblarQ2(programa_valido)
  }

  def programas = new {
    var instrucciones = List(MOV(R0, new Directo(new Inmediato("0002"))), MUL(R4, new Inmediato("0001")), SUB(new Directo(new Inmediato("0003")), new Inmediato("000A")),
      ADD(R5, new Inmediato("0056")), MOV(new Directo(new Inmediato("000B")), new Directo(new Inmediato("0005"))), ADD(R1, R7))
    var programaQ2 = new Programa(instrucciones)
    var instruccionesinterpretadas = List("1808 0002", "0900 0001", "3200 0003 000A", "2940 0056", "1208 000B 0005", "2867 ")

    var instruccionesdecodificadas = List("MOV R0, [0x0002]", "MUL R4, 0x0001", "SUB [0x0003], 0x000A", "ADD R5, 0x0056", "MOV [0x000B], [0x0005]", "ADD R1, R7")
  }
  //--------------------------------------------TESTS PARSER -----------------------------------------------//

  "Un Parser" should "parsear exitosamente un programa " in {
    var set_parser = parsers_resultados
    var set_programas = programas

    assert(set_parser.resultadoQ2.equals(set_programas.programaQ2))
  }

  it should "tirar un Failure cuando parsea un programa con sintaxis invalida" in {
    var set_parser = parsers_resultados
    var set_programas = programas

    var mensaje_esperado = "Ha ocurrido un error en la linea 3 : SUB [], 0x000A"
    val exception = intercept[SyntaxErrorException] {
      set_parser.parser.ensamblarQ2(set_parser.programa_invalido)
    }
    assert(exception.getMessage().equals(mensaje_esperado))
  }

  //----------------------------------------------TESTS SIMULADOR -----------------------------------------------//

  def simuladores = new {
    var parser = parsers_resultados
    var programa = parser.resultadoQ2
    var registros_actualizar = registros_a_actualizar

    var simulador = new Simulador()
    simulador.inicializarSim()

    var simulador_con_programa = new Simulador()
    simulador_con_programa.inicializarSim()
    simulador_con_programa.cargarProgramaYRegistros(programa, "0000", registros_actualizar.registros)
  }

  def registros_a_actualizar = new {
    var registros = Map[String, W16](("R5", "0010"), ("R0", "0010"), ("R2", "9800"), ("R1", "0009"), ("R7", "0001"))
  }

  "Un Simulador" should "cargar un programa en la memoria desde la posicion que indica pc" in {
    var set_simuladores = simuladores
    var set_registros = registros_a_actualizar
    var set_parser = parsers_resultados
    var pc = "0000"
    var programa = set_parser.resultadoQ2

    set_simuladores.simulador.cargarProgramaYRegistros(programa, pc, set_registros.registros)

    set_simuladores.simulador.cpu.pc.hex should be(pc)
  }

  it should "actualizar los registros de cpu" in {
    var set_simuladores = simuladores
    var set_registros = registros_a_actualizar
    var mapaRegistros = set_registros.registros
    // verificando que los registros se actualicen bien
    for {
      key ← mapaRegistros.keys
      value = mapaRegistros(key)
    } yield {
      set_simuladores.simulador.cpu.registro(key) match {
        case Some(registro) ⇒ {
          assert(registro.valor.equals(value))
        }
        case _ ⇒
      }
    }
  }
  //-----------------------------------------------------EJECUCION PASO A PASO -----------------------------------------//
  it should "ejecutar el ciclo de instruccion FETCH - DECODE (Paso-a-Paso) al programa que esta cargado en la memoria " in {
    var set_simuladores = simuladores
    var set_parser = parsers_resultados
    var programa = set_parser.resultadoQ2
    var instrucciones = programas
    var count = 0
    do {
      //FETCH
      set_simuladores.simulador_con_programa.fetch()
      assert(instrucciones.instruccionesinterpretadas(count) === set_simuladores.simulador_con_programa.cpu.ir)
      //DECODE
      var decode = set_simuladores.simulador_con_programa.decode()
      assert(instrucciones.instruccionesdecodificadas(count) === decode)

      count += 1
    } while (count < programa.instrucciones.length)

  }
}