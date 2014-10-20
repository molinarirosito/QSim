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

import org.uqbar.commons.utils.Observable

object State extends Enumeration {
  type Type = Value
  val NONE, PROGRAM, FECH_DECODE, STORE, EXECUTED = Value
}

@Observable
class Celda (var value: W16) {
  var state = State.NONE
  
  override def toString() =  value.toString
  
  def setW16(word16: W16) = this.value = word16
  def getW16():  W16 = this.value 
 
   

}