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
import scala.collection.mutable._
import ar.edu.unq.tpi.qsim.parser.Parser

class EjecucionInstruccionesLlamadaASubRutinaYSaltos extends FlatSpec with Matchers {

  def contexto_programas = new {
    var path : PathTest = PathTest()
    var programaCALLRET = path.getContenido("CALLRET", 1)
    var programa_salt = path.getContenido("CALLRET", 2)
    var parser = Parser
    var programa = parser.ensamblarQ3(programaCALLRET)
    var programa_saltos = parser.ensamblarQ4(programa_salt)
  }

  def simuladores = new {
    var set_contexto_programas = contexto_programas
    var simulador = Simulador()
    simulador.inicializarSim
    simulador.cargarProgramaYRegistros(set_contexto_programas.programa, "0000", Map[String, W16]())

    // Ejecuto la primera instruccion para que quede Call para ejecutar
    simulador.fetch()
    simulador.decode()
    simulador.execute()

    var simuladorRET = Simulador()
    simuladorRET.inicializarSim
    simuladorRET.cargarProgramaYRegistros(set_contexto_programas.programa, "0000", Map[String, W16]())

    // Ejecuto la primera instruccion para que quede RET para ejecutar
    for (x ← 0 to 2) {
      simuladorRET.fetch()
      simuladorRET.decode()
      simuladorRET.execute()
    }
    var simuladorSaltos = Simulador()
    simuladorSaltos.inicializarSim
    simuladorSaltos.cargarProgramaYRegistros(set_contexto_programas.programa_saltos, "0000", Map[String, W16]())
     // Ejecuto la primera instruccion para que quede JMP para ejecutar
    simuladorSaltos.fetch()
    simuladorSaltos.decode()
    simuladorSaltos.execute()
  }

  "Un CALL" should "guardar el valor del pc actual en la pila y actualizar el valor de pc con el operado origen" in {
    // Ejecutando la segunda instruccion que es el call
    var set_simuladores = simuladores
    var simulador = set_simuladores.simulador
    var pcActual = new W16("0004")
    var spActual = new W16("FFEF")
    simulador.fetch()
    var instruccion = simulador.instruccionActual.asInstanceOf[Instruccion_UnOperando_Origen]
    simulador.decode()
    simulador.execute()
    // probar que la ejecucion de la instruccion Call funciona    
    assert(simulador.busIO.getValor(spActual).value === pcActual.value)
    assert(simulador.cpu.pc.equals(instruccion.operando.getValor))
  }

  "Un RET" should "buscar el valor del tope de la pila y actualizar el pc con ese valor" in {
    var set_simuladores = simuladores
    var simulador = set_simuladores.simuladorRET
    var pcActual = new W16("0004")
    var spActual = new W16("FFEF")

    simulador.fetch()
    var instruccion = simulador.instruccionActual.asInstanceOf[Instruccion_SinOperandos]
    simulador.decode()
    simulador.execute()
    // probar que la ejecucion de la instruccion RET funciona    
    assert(simulador.cpu.sp.equals(spActual))
    assert(simulador.cpu.pc.equals(pcActual))

  }

  "Un JMP" should "enviar el hilo de ejecucion a la celda que indica el valor del operando origen que recibe -(cambiar pc por el valor del operando)" in {
    var set_simuladores = simuladores
    var simulador = set_simuladores.simuladorSaltos
    simulador.fetch()
    var instruccion = simulador.instruccionActual.asInstanceOf[Instruccion_UnOperando_Origen]
    simulador.decode()
    simulador.execute()
    // probar que la ejecucion de la instruccion JMP funciona    
    assert(simulador.cpu.pc.equals(instruccion.operando.getValor))
  }

//  "Un lal" should "buscar el valor del tope de la pila y actualizar el pc con ese valor" in {
//    var set_simuladores = simuladores
//    var simulador = set_simuladores.simuladorRET
//    var pcActual = new W16("0004")
//    var spActual = new W16("FFEF")
//
//    simulador.fetch()
//    var instruccion = simulador.instruccionActual.asInstanceOf[Instruccion_SinOperandos]
//    simulador.decode()
//    simulador.execute()
//    // probar que la ejecucion de la instruccion RET funciona    
//    assert(simulador.cpu.sp.equals(spActual))
//    assert(simulador.cpu.pc.equals(pcActual))
//
//  }

}