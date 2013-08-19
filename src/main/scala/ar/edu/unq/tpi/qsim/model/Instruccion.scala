package ar.edu.unq.tpi.qsim.model
import ar.edu.unq.tpi.qsim.utils._

class Instruccion(val codigoDeOperacion: String, var operacion: String, var destino: ModoDireccionamiento, var origen: ModoDireccionamiento){
//ESTA MAL PORQUE TENGO QUE TOMAR LAS COSAS JUNTAS, UN ADD R0 R1 OCUPA SOLO 4 HEXA BITS
//Y A MI ME ESTA OCUPANDO 3!!!!!
def representacionHexadecimal() : String  =  {
  val codigoOperacion_hex = Util.binaryToHex(codigoDeOperacion)
  val destiny_code = Util.binaryToHex(destino.codigo)
  val destiny_value = destino.getValorString
  
  val origin_code = Util.binaryToHex(origen.codigo)
  val origin_value = origen.getValorString
    
  (codigoDeOperacion + " " + destiny_code + " " + origin_code + " " + destiny_value + " " + origin_value).replace("  "," ")
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


object Testud extends App{


  val e = ADD(R1,R5)
  println(e.toString())
  println(e.representacionHexadecimal())
  println(e.tamanioHex)
}
