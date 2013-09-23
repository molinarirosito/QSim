package ar.edu.unq.tpi.qsim.parser

import ar.edu.unq.tpi.qsim.model.Programa

object Parser extends Ensamblador {

  def ensamblar(path: String) = {
    val input = io.Source.fromFile(path)
    val str = input.mkString

    parse(str) match {
      case Success(result, _) =>  OK(result)
      case Failure(msg, i) => FAILURE("[Failure] " + s" $msg in $i")
      case Error(msg, i) => FAILURE("[Error] " + s" $msg in $i")
    }
  }
// TODO ver si se puede agregar un metodo para saber el estado de salida que tuvo el parser!!
}

abstract class Result{}

case class OK(var resultado: Programa) extends Result {}

case class FAILURE(var mensaje: String) extends Result {}
  
