package ar.edu.unq.tpi.qsim.model

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

import ar.edu.unq.tpi.qsim.utils._

class Salto(var salto: Int) {

  /**
   * Devuelve el numero que tiene el salto en String
   * @return String
   */
  override def toString() = value.toString

  /**
   * Devuelve la representacion binaria del numero que tiene el salto
   * @return String
   */
  def toBinary: String = Util.toBinary8B(salto)

  /**
   * Devuelve el numero que tiene el salto
   * @return Int
   */
  def value: Int = salto

  def this(binary_chain: String) {
    this(Util.binaryToInteger(binary_chain));

  }
}

class SaltoEtiqueta(var etiqueta: ModoDireccionamiento) extends Salto(0) {
}