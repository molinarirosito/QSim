package ar.edu.unq.tpi.qsim.parser

import ar.edu.unq.tpi.qsim.model.Programa
import scala.util.parsing.input.CharSequenceReader

object Parser extends Ensamblador {

  def readFile(path: String): String = {
    val input = io.Source.fromFile(path)
    return input.mkString
  }

  def ensamblarQ1(path: String): Result = {
    val str = readFile(path)
    result(parse(str, this.programQ1))
  }
  def ensamblarQ2(path: String): Result = {
    val str = readFile(path)
    result(parse(str, this.programQ2))
  }

  def result(resultado: ParseResult[Programa]): Result = resultado match {
    case Success(result, _) ⇒ OK(result)
    case Failure(msg, i) ⇒ {
      var mensaje = definirMensajeError(i)
      FAILURE(s"$mensaje")
    }
    case Error(msg, i) ⇒ FAILURE(s"$msg")
  }
  def definirMensajeError(output: Input): String = {
    var characterCount = output.offset
    var lineOfProgram = output.source.toString().split("\n")
    var mensaje = ""
    var countCharaters = 0
    lineOfProgram.foreach(line ⇒ {
      if (characterCount >= countCharaters && characterCount <= countCharaters + line.length()) {
        var countLine = (lineOfProgram.indexOf(line) + 1).toString
        mensaje = s"A ocurrido un error en la linea $countLine $line"
      }
      countCharaters += line.length()
    })
    return mensaje
  }
}

abstract class Result(var estado: String) {}

case class OK(var resultado: Programa) extends Result("OK") {}

case class FAILURE(var mensaje: String) extends Result("FAILURE") {}
  
