package ar.edu.unq.tpi.qsim.model
import scala.collection.mutable.ArrayBuffer
import ar.edu.unq.tpi.qsim.utils._


case class Memoria(var tamanio: Int) {
  
  var celdas : ArrayBuffer[String] = _
  
  def tamanioMemoria() : Int = tamanio
  
  def initialize() = {
    celdas = new ArrayBuffer[String](tamanio)
    var contador = 0
    do{
       celdas+= ("0000")
         contador = contador + 1
      }while( contador < 65536 )
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
  
  def cargarPrograma(programa: Programa, pc:String){
   //Quiero hacerlo de esta forma pero tengo que pensar bien las cosas
    //programa.instrucciones.foreach(instruccion => setValor(pc, instruccion.representacionHexadecimal()))
    var celda_pc = Util.hexToInteger(pc)
    for (x <- 0 to programa.instrucciones.size - 1){
  		if(celda_pc < Memoria.this.tamanioMemoria()){   
  		  setValorC(celda_pc,programa.instrucciones(x).representacionHexadecimal())
  		}
	   celda_pc= celda_pc + 1
  	}	
  }
   //tuve que agregar este metodo para agilizar las cosas pero habria que pensar que seria lo mejor. 
   def setValorC(celda: Int, valor: String) =
  {  celdas(celda) = valor}
    
  
  def setValor(celda: String, valor: String) =
  {
    val numero_celda = Util.hexToInteger(celda)
    celdas(numero_celda) = valor
    
  }
  
}

object Testing extends App{
  var memory = Memoria(65536)
  var a = 0
   memory.initialize
  println(memory.celdas)
   println(memory.celdas.size)
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
