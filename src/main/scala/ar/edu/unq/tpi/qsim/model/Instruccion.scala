package ar.edu.unq.tpi.qsim.model
import ar.edu.unq.tpi.qsim.utils._

class Instruccion(val codigoDeOperacion: String, var operacion: String, var destino: ModoDireccionamiento, var origen: ModoDireccionamiento){

def representacionHexadecimal() : String  =  {
 
  val operations_code_binary = Util.binary16ToHex(codigoDeOperacion + destino.codigo + origen.codigo)
   
  val destiny_value = destino.getValorString
  val origin_value = origen.getValorString
    
  (operations_code_binary + " " + destiny_value + " " + origin_value).replace("  "," ")
}

def decode() : String = Instruccion.this.toString 
def tamanioHex() : Int = Instruccion.this.representacionHexadecimal.replace(" ","").size
def cantidadCeldas() : Int = Instruccion.this.tamanioHex/4

 override def toString() =  operacion + " " + destino.toString() + " " + origen.toString() 
  
}

case class MUL(dest: ModoDireccionamiento, orig: ModoDireccionamiento) extends Instruccion("0000","MUL",dest,orig){

}

case class ADD(dest: ModoDireccionamiento, orig: ModoDireccionamiento) extends Instruccion("0010","ADD",dest,orig){
}

case class SUB(dest: ModoDireccionamiento, orig: ModoDireccionamiento) extends Instruccion("0011","SUB",dest,orig){
}

case class DIV(dest: ModoDireccionamiento, orig: ModoDireccionamiento) extends Instruccion("0111","DIV",dest,orig){
}

case class MOV(dest: ModoDireccionamiento, orig: ModoDireccionamiento) extends Instruccion("0001","MOV",dest,orig){
}


object Testudd extends App{


  val e = ADD(R1,R5)
  println(e.toString())
  println(e.representacionHexadecimal())
  println(e.tamanioHex)
}
