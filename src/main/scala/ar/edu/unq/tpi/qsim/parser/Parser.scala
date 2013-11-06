package ar.edu.unq.tpi.qsim.parser

import ar.edu.unq.tpi.qsim.model.Programa
import scala.util.parsing.input.CharSequenceReader
import ar.edu.unq.tpi.qsim.exeptions.SyntaxErrorException

object Parser extends Ensamblador {

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

  def ensamblarQ3SDFADSDFDSFASFASDFASDFASDFASD(codigo: String): Programa = {
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
    val str = readFile(path)
    result(parse(str, this.programQ6))
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
    var lineOfProgram = output.source.toString().split("\n")
    var characterCount = output.offset - (lineOfProgram.length - 1)

    return searchLineWithError(lineOfProgram, characterCount)
  }

  def searchLineWithError(lineOfProgram: Array[String], amountCharactersBeforeError: Int): String = {
    var countCharaters = 0
    var mensaje = ""
    lineOfProgram.foreach(line ⇒ {
      if (amountCharactersBeforeError >= countCharaters && amountCharactersBeforeError <= countCharaters + line.length()) {
        var countLine = (lineOfProgram.indexOf(line) + 1).toString
        mensaje = s"Ha ocurrido un error en la linea $countLine $line"
      }
      countCharaters += line.length()
    })
    return mensaje
  }
}