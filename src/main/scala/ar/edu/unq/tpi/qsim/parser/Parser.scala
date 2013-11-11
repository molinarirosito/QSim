package ar.edu.unq.tpi.qsim.parser

import ar.edu.unq.tpi.qsim.model.Programa
import scala.util.parsing.input.CharSequenceReader
import ar.edu.unq.tpi.qsim.exeptions.SyntaxErrorException
import org.uqbar.commons.utils.Observable

object Parser extends Ensamblador {

  var arquitecturas = List(ArquitecturaQ("Q1", ensamblarQ1), ArquitecturaQ("Q2",ensamblarQ2), ArquitecturaQ("Q3", ensamblarQ3), ArquitecturaQ("Q4", ensamblarQ4), ArquitecturaQ("Q5", ensamblarQ5), ArquitecturaQ("Q6", ensamblarQ6))
  
  def readFile(path: String): String = {
    val input = io.Source.fromFile(path)
    return input.mkString
  }

  def ensamblarQ1(path: String): Programa = {
    val str = readFile(path)
    result(parse(str, this.programQ1))
  }

  def ensamblarQ2(path: String): Programa = {
    val str = readFile(path)
    result(parse(str, this.programQ2))
  }

  def ensamblarQ3(path: String): Programa = {
    val str = readFile(path)
    result(parse(str, this.programQ3))
  }

  def ensamblarQ3SDFADSDFDSFASFASDFASDFASDFASDAccionesAccionesAccionesAccionesAccionesAccionesAccionesAcciones(codigo: String): Programa = {
    result(parse(codigo, this.programQ3))
  }

  def ensamblarQ4(path: String): Programa = {
    val str = readFile(path)
    result(parse(str, this.programQ4))
  }

  def ensamblarQ5(path: String): Programa = {
    val str = readFile(path)
    result(parse(str, this.programQ5))
  }
  def ensamblarQ6(path: String): Programa = {
    //val str = readFile(path)
    result(parse(path, this.programQ6))
  }

  def result(resultado: ParseResult[Programa]): Programa = resultado match {
    case Success(result, _) ⇒ result
    case Failure(msg, i) ⇒ {
     var mensaje = createMessage(i)
     throw new SyntaxErrorException(mensaje)
    }
    case Error(msg, i) ⇒ throw new SyntaxErrorException(msg)
  }

  def createMessage(output: Input): String = {
    var characterCount = output.offset 
    var cadenaCaracteres = output.source
    var lineas = output.source.toString().split("\n")
    return lineWithError(cadenaCaracteres.toString().substring(0, characterCount),lineas)

  } 

  def lineWithError(cadenaConError: String, lineas : Array[String] ): String = {
    var lineasCortadasEnError = cadenaConError.split("\n")
    var numeroLinea = lineasCortadasEnError.length
    var linea = lineas(numeroLinea -1)
    return s"Ha ocurrido un error en la linea $numeroLinea : $linea"
  }
}

@Observable
case class ArquitecturaQ(var name: String, parser:(String)=>Programa) {
  override def toString = name
}