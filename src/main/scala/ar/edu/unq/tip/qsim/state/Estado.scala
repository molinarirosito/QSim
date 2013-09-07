package ar.edu.unq.tip.qsim.state

import ar.edu.unq.tpi.qsim.model.CPU
import ar.edu.unq.tpi.qsim.model.Memoria
import ar.edu.unq.tpi.qsim.model.Programa

abstract class Estado {
  def actualizarRegistros(registros: Map[String, Any]): Unit
}

class Inicial(var cpu: CPU) extends Estado {

  def actualizarRegistros(registros: Map[String, _]) {
    cpu.c = registros("c").asInstanceOf[Integer]
    cpu.n = registros("n").asInstanceOf[Integer]
    cpu.v = registros("v").asInstanceOf[Integer]
    cpu.z = registros("z").asInstanceOf[Integer]
    for (x <- 0 to 7) {
      if (registros.contains("R" + x)) {
        var registro = cpu.registros(x)
      //  registro.valor = registros("R" + x).asInstanceOf[String]
        cpu.registros(x) = registro
      }
    }
  }
}

class Fetch(var cpu:CPU) extends Estado() {

  def actualizarRegistros(registros: Map[String, Any]) {
	  cpu.ir = registros("ir").asInstanceOf[String]
  }
}

class Decode extends Estado {

  def actualizarRegistros(registros: Map[String, Any]) {

  }
}

class Execute extends Estado {

  def actualizarRegistros(registros: Map[String, Any]) {

  }

}