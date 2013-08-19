package ar.edu.unq.tpi.qsim.model

import ar.edu.unq.tip.qsim.state.Fetch
import ar.edu.unq.tip.qsim.state.Decode
import ar.edu.unq.tip.qsim.state.Execute

class UnidadControl(var cpu: CPU) {

  def ejecutarCicloInstruccion(instruccion: Instruccion) {
    fetch(instruccion)
    decode(instruccion)
  }

  def fetch(instruccion: Instruccion) {
    cpu.cambiarEstado(new Fetch(cpu))
    cpu.estado.actualizarRegistros(Map("ir" -> instruccion.representacionHexadecimal().asInstanceOf[Any]))
    cpu.incrementarPc()

  }

  def decode(instruccion: Instruccion) {
    cpu.cambiarEstado(new Decode())
    cpu.estado.actualizarRegistros(Map("irDecode" -> instruccion.decode().asInstanceOf[Any]))
  }

  def execute(instruccion: Instruccion) {
    if(!(instruccion.operacion.equals("MOV"))){
    	// llamar a la alu para que realice la operacion
    } 
    // tiene que aver algo que identifique si tiene que ir a memoria a guardar el dato o solamente ir a registro.
    // necesariamente necesitamos algo que conecte la cpu con la memoria y para eso estan los buses :D
    // aca le tiene que decir el destino de la instruccion que se actualice con el valor resultante de la operacion de la alu.
    cpu.cambiarEstado(new Execute())
    cpu.estado.actualizarRegistros(Map("R0" -> "0009"))// el map que recibe este metodo lo saca de a alu
    

  }

}