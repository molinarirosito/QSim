package ar.edu.unq.tpi.qsim.parser

import ar.edu.unq.tpi.qsim.model.Programa

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
    case Failure(msg, i) ⇒ FAILURE(s"$msg") //in $i")
    case Error(msg, i) ⇒ FAILURE(s"$msg") //in $i")
  }
}

abstract class Result(var estado: String) {}

case class OK(var resultado: Programa) extends Result("OK") {}

case class FAILURE(var mensaje: String) extends Result("FAILURE") {}
  
