package ar.edu.unq.tpi.qsim.exeptions
import org.uqbar.commons.model.UserException

import scala.util.control.Exception

case class EtiquetaInvalidaException(smth:String) extends UserException(smth) {

}

case class DesplazamientoSaltoInvalidoException(smth:String) extends UserException(smth) {

}

case class CodigoInvalidoException(smth:String) extends UserException(smth) {

}

case class CeldaFueraDeMemoriaException(smth:String) extends UserException(smth) {

}

case class CeldaFueraDePuertosException(smth:String) extends UserException(smth) {

}

case class ModoDeDireccionamientoInvalidoException(smth:String) extends UserException(smth) {

}

case class SyntaxErrorException(smth:String) extends UserException(smth) {

}