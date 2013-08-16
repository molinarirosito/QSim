package parser


object PruebaImplicit extends App{
  
	implicit class Pers(val nombre:String){
		def hola = " hola" 
	}

	print ("sfdf" hola )
	
}