package ar.edu.unq.tpi.qsim.parser

import ar.edu.unq.tpi.qsim.model.Programa

object Parser extends Ensamblador {

  def ensamblar(path: String) :Result = {
    val input = io.Source.fromFile(path)
    val str = input.mkString

    parse(str) match {
      case Success(result, _) =>  OK(result)
      case Failure(msg, i) => 
        FAILURE(s" $msg in $i")
      case Error(msg, i) => FAILURE("[Error] " + s" $msg in $i")
    }
  }
}

abstract class Result(var estado: String){}

case class OK(var resultado: Programa) extends Result("OK") {}

case class FAILURE(var mensaje: String) extends Result("FAILURE") {}
  
