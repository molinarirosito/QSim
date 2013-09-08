package ar.edu.unq.tip.qsim.state

import ar.edu.unq.tpi.qsim.model.CPU
import ar.edu.unq.tpi.qsim.model.Memoria
import ar.edu.unq.tpi.qsim.model.Programa
import ar.edu.unq.tpi.qsim.model.W16

abstract class Estado {
  def actualizarRegistros(registros: Map[String, W16]): Unit
}

class Inicial(var cpu: CPU) extends Estado {

  def actualizarRegistros(registros: Map[String, W16]) {
    for (x ← 0 to 7) {
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