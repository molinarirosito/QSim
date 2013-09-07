package ar.edu.unq.tpi.qsim.model

import ar.edu.unq.tpi.qsim.utils.Util

trait ModoDireccionamiento {
  //DELEGAR CAMBIO A EXADECIMAL A LA INSTRUCCION, SE SOLAPAN LOS MODOS DE DIRECCIONAMIENTO!!!

  def getValor() : W16
  def codigo() : String
  def representacionString() : String
  override def toString() =  this.representacionString()
  def getValorString() : String
  def bits() : Int = 0
}

abstract class Registro() extends ModoDireccionamiento{

var valor : W16 = new W16("0000")

def numero(): Int
def representacionString() :String = "R" + numero
override def bits() : Int = 6
override def getValorString() : String = valor.toString
def getValor() : W16 = valor
def setValor(un_valor : W16){
  valor = un_valor
}

def codigo() :String =
{ 
  "100" + Util.toBinary3B(numero)
}
}

object R0 extends Registro()
{ 
  override def numero : Int = 0
}
object R1 extends Registro()
{ 
  override def numero : Int = 1
}
object R2 extends Registro()
{ 
  override def numero : Int = 2
}
object R3 extends Registro()
{ 
  override def numero : Int = 3
}
object R4 extends Registro()
{ 
  override def numero : Int = 4
}
object R5 extends Registro()
{ 
  override def numero : Int = 5
}
object R6 extends Registro()
{ 
  override def numero : Int = 6
}
object R7 extends Registro()
{ 
  override def numero : Int = 7
}


case class Inmediato (valor : W16) extends ModoDireccionamiento{
  def getValor() : W16 = valor
  override def getValorString() : String = valor.toString
  def representacionString() :String = valor.toString
  def codigo() :String  = "000000"
 // override def bits() : Int = this.toBinaryString.size - 1
}
