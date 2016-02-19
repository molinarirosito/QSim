package ar.edu.unq.tpi.qsim.parser

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


import ar.edu.unq.tpi.qsim.model.Programa
import scala.util.parsing.input.CharSequenceReader
import ar.edu.unq.tpi.qsim.exeptions.SyntaxErrorException
import org.uqbar.commons.utils.Observable
import ar.edu.unq.tpi.qsim.integracion.mumuki.JsonResult

object Parser extends Ensamblador {

  var arquitecturas = List(ArquitecturaQ("Q1", ensamblarQ1), ArquitecturaQ("Q2", ensamblarQ2), ArquitecturaQ("Q3", ensamblarQ3), ArquitecturaQ("Q4", ensamblarQ4), ArquitecturaQ("Q5", ensamblarQ5), ArquitecturaQ("Q6", ensamblarQ6))
  var jsonResult = new JsonResult()
  
  def ensamblarQ1(code: String): Programa = result(parse(code, this.programQ1))

  def ensamblarQ2(code: String): Programa = result(parse(code, this.programQ2))

  def ensamblarQ3(code: String): Programa = result(parse(code, this.programQ3))

  def ensamblarQ4(code: String): Programa = result(parse(code, this.programQ4))

  def ensamblarQ5(code: String): Programa = result(parse(code, this.programQ5))

  def ensamblarQ6(code: String): Programa = result(parse(code, this.programQ6))

  def result(resultado: ParseResult[Programa]): Programa = resultado match {
    case Success(result, _) ⇒ result
    case Failure(msg, i) ⇒ {
      var mensaje = createMessage(i)
      jsonResult.buildJsonError(mensaje)
      throw new SyntaxErrorException(mensaje)
    }
    case Error(msg, i) ⇒ throw new SyntaxErrorException(msg)
  }

  def createMessage(output: Input): String = {
    var characterCount = output.offset
    var cadenaCaracteres = output.source
    var lineas = output.source.toString().split("\n")
    var numeroLinea = buscarLineaConError(lineas, characterCount)
    return lineaResultado(numeroLinea, lineas)
  }

  def buscarLineaConError(lineas: Array[String], characterError: Int): Int = {
    var lineaConErrorBordeIzq = 0
    var cantCaracteres = 0
    var encontreLinea = false
    var i = 0
    while (i < lineas.size && !(encontreLinea)) {
      var linea = lineas(i)
      cantCaracteres = linea.length() + cantCaracteres + 1
      if (siLineasPrincipioSoloIntrSinOPoUnOpSonLineasConError(linea) || cantCaracteres > 0 && cantCaracteres > characterError ) {
        lineaConErrorBordeIzq = i + 1
        encontreLinea = true
      }
      i = i + 1
    }
    return lineaConErrorBordeIzq
  }

  def siLineasPrincipioSoloIntrSinOPoUnOpSonLineasConError(linea: String): Boolean = {
    var estaNotoJMP = false
    if (linea.length() <= 4) {
      if (!(linea.trim().contains("RET")) || linea.trim().equals("JMP") || linea.trim().equals("NOT")) {
        estaNotoJMP = true
      }
    }
    return estaNotoJMP
  }

  def lineaResultado(numeroLinea: Int, lineas: Array[String]): String = {
    var linea = lineas(numeroLinea - 1)
    s"Ha ocurrido un error en la linea $numeroLinea : $linea"
  }

}

@Observable
case class ArquitecturaQ(var name: String, parser: (String) ⇒ Programa) {
  override def toString = name
}

object pruebaError extends App {
  
    var string = """NOT 
AND [R6], [0x0006]
OR [[0x0023]], 0x0300
AND R5, [0x0005]
OR R6, 0x0005"""
  var parser = Parser
  try {
    parser.ensamblarQ6(string)
  } catch {
    case ex: SyntaxErrorException ⇒ {
      print(ex.getMessage())
    }
  }

} 