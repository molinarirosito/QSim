package ar.edu.unq.tpi.qsim.beans
import scala.collection.mutable.ArrayBuffer
import ar.edu.unq.tpi.qsim.utils._


case class Memoria(var tamanio: Int) {
  
  var celdas : ArrayBuffer[String] = _
  
  def tamanioMemoria() : Int = tamanio
  
  def initialize() = {
    
    celdas = new ArrayBuffer[String](tamanio)
    celdas = celdas.map(i => "0000")
  }
  
  def getValor(pc : String) : String = {
    var celda_actual = Util.hexToInteger(pc)
    if(celda_actual< Memoria.this.tamanioMemoria())
    {   var value = celdas(celda_actual)
    	//pc = Util.toHex(current_cel + 1)
    	value    }
    else
    	"Esta no es una celda de memoria valida!!"
  }
  
  def setValor(celda: String, valor: String) =
  {
    val numero_celda = Util.hexToInteger(celda)
    celdas(numero_celda) = valor
    
  }
  
}

object Testing extends App{
  var memory = Memoria(5)
  memory.initialize
  print(memory.celdas)
  /*memory.cells = ArrayBuffer("1000","1200","1300","1400","1000","1200","1300","1400")
  
  var value = memory.getValue("0001")
  print("el valor es" )
  print("\n")
  print(value)
  value = memory.getValue("0002")
  memory.cells(1)= "ola"
    print("\n")
    print(memory.cells)*/
  
  

}
