package ar.edu.unq.tpi.qsim.exeptions

/**
* Copyright 2014 Tatiana Molinari.
* Copyright 2014 Susana Rosito
*
* This program is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program. If not, see <http://www.gnu.org/licenses/>.
*
*/

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

case class StackPointerExeption(smth:String) extends UserException(smth) {

}