package ar.edu.unq.tip.qsim.state


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


import ar.edu.unq.tpi.qsim.model.CPU
import ar.edu.unq.tpi.qsim.model.Memoria
import ar.edu.unq.tpi.qsim.model.Programa
import ar.edu.unq.tpi.qsim.model.W16

abstract class Estado {
  def actualizarRegistros(registros: Map[String, W16]): Unit
}

class Inicial(var cpu: CPU) extends Estado {

  def actualizarRegistros(registros: Map[String, W16]) {
    for (x <- 0 to 7) {
      if (registros.contains("R" + x))
    	  cpu.registros(x).valor = registros("R" + x)
       
    }
  }
}
class Fetch(var cpu: CPU) extends Estado() {

  def actualizarRegistros(registros: Map[String, W16]) {
    cpu.ir = registros("ir").asInstanceOf[String]
  }
}

class Decode extends Estado {

  def actualizarRegistros(registros: Map[String, W16]) {

  }
}

class Execute extends Estado {

  def actualizarRegistros(registros: Map[String, W16]) {

  }

}