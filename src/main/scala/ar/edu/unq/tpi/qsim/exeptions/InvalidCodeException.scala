package ar.edu.unq.tpi.qsim.exeptions

import scala.util.control.Exception

case class InvalidCodeException(smth:String) extends Exception {

}