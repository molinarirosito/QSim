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
  
  def cargarPrograma(programa: Programa, inicio:String){
   //Quiero hacerlo de esta forma pero tengo que pensar bien las cosas
    //programa.instrucciones.foreach(instruccion => setValor(pc, instruccion.representacionHexadecimal()))
  
    var celda_inicio = Util.hexToInteger(inicio)
    val celda_fin = celda_inicio + programa.tamanioDelPrograma
    if (celda_fin < Memoria.this.tamanioMemoria()){
    for (x <- 0 to programa.instrucciones.size - 1){
  	  setValorC(celda_inicio,programa.instrucciones(x).representacionHexadecimal()) }
	   celda_inicio= celda_inicio + 1
  	}	
  }
   
  
  def setValorC(celda: Int, valor: String) =
  {  celdas(celda) = valor }
    
  
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
