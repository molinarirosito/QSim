package ar.edu.unq.tpi.qsim.exeptions

import scala.util.control.Exception

case class CodigoInvalidoException(smth:String) extends Exception(smth) {

}

case class CeldaFueraDeMemoriaException(smth:String) extends Exception(smth) {

}

case class CeldaFueraDePuertosException(smth:String) extends Exception(smth) {

}

case class SintaxErrorException(smth:String) extends Exception(smth) {

}