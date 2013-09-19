package ar.edu.unq.tpi.qsim.exeptions

import scala.util.control.Exception

case class CodigoInvalidoException(smth:String) extends Exception {

}

case class CeldaFueraDeMemoriaException(smth:String) extends Exception {

}