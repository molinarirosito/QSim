package ar.edu.unq.tpi.qsim.model
import ar.edu.unq.tpi.qsim.utils.Util
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer

object ALU {

  /**
   * Ejecuta la operacion matematica pasada por parametro como funcion de dos operandos y
   * obtiene el resultado que devuelve en un map junto con los flags comunes a todas las operaciones actualizados.
   * @params operacion: (Int, Int) => Int, op1: W16, op2: W16.
   * @return Map[String, Any]
   */
  def execute_operacion_matematica(operacion: (Int, Int) ⇒ Int, op1: W16, op2: W16): Map[String, Any] = {
    val valor = operacion(op1.value, op2.value)
    val resultado_binario = Util.toBinary16BOverflow(valor)
    val flags = takeFlags(valor)

    var resultado = new W16(Util.fromBinaryToHex4(resultado_binario))
    Map(("resultado", resultado), ("valor", valor), ("resultado_binario", resultado_binario) ,("n", flags._1), ("z", flags._2), ("c", 0), ("v", 0))

  }

  /**
   * Devuelve los Flags Negative y Zero en respectivo orden tomando un valor entero.
   * @params valor: Int
   * @return (Int, Int)
   */
  def takeFlags(valor: Int): (Int, Int) = (actualizarNegative(valor), actualizarZero(valor))

  /**
   * Devuelve los Flags Carry y Overflow en la suma, en respectivo orden tomando un valor W16.
   * @params resultado_binario: W16, valor: Int, op1: W16, op2: W16
   * @return (Int, Int)
   */
  def takeFlagsSum(resultado: W16, resultado_binario: String ,valor: Int, op1: W16, op2: W16): (Int, Int) = {
    val c = actualizarCarry(resultado_binario)
    val v = verificarCondicionOverflowSuma(resultado, op1, op2)
    (c, v)
  }

  /**
   * Devuelve los Flags Carry y Overflow en la resta, en respectivo orden tomando un valor W16.
   * @params resultado_binario: W16, valor Int, op1: W16, op2: W16
   * @return (Int, Int)
   */
  def takeFlagsRest(resultado: W16, valor: Int, op1: W16, op2: W16): (Int, Int) = {
    val c = actualizarBorrow(valor)
    val v = verificarCondicionOverflowResta(resultado, op1, op2)
    (c, v)
  }

  /**
   * Simula la ejecucion del ADD con dos W16 y actualiza los flags
   * @params op1: W16, op2: W16
   * @return Map[String, Any]
   */
  def execute_add(op1: W16, op2: W16): Map[String, Any] = {
    var resultados = execute_operacion_matematica(_ + _, op1, op2)
    val carryOverflow = takeFlagsSum(resultados("resultado").asInstanceOf[W16], resultados("resultado_binario").asInstanceOf[String],resultados("valor").asInstanceOf[Int], op1, op2)
    guardarResultadosCarryOverflow(resultados, carryOverflow)
  }

  /**
   * Simula la ejecucion del SUB con dos W16 y actualiza los flags
   * @params op1: W16, op2: W16
   * @return Map[String, Any]
   */
  def execute_sub(op1: W16, op2: W16): Map[String, Any] = {
    var resultados = execute_operacion_matematica(_ - _, op1, op2)
    val carryOverflow = takeFlagsRest(resultados("resultado").asInstanceOf[W16],resultados("valor").asInstanceOf[Int], op1, op2)
    guardarResultadosCarryOverflow(resultados, carryOverflow)
  }
  /**
   * Simula la ejecucion del MUL con dos W16 y actualiza los flags
   * @params op1: W16, op2: W16
   * @return Map[String, Any]
   */
  def execute_mul(op1: W16, op2: W16): Map[String, Any] = {
    execute_operacion_mul(_ * _, op1, op2)
  }

  /**
   * Simula la ejecucion del DIV con dos W16 y actualiza los flags
   * @params op1: W16, op2: W16
   * @return Map[String, Any]
   */
  def execute_div(op1: W16, op2: W16): Map[String, Any] = {
    execute_operacion_matematica(_ / _, op1, op2)
  }

  /**
   * Simula la ejecucion del CMP con dos W16 y actualiza los flags
   * @params op1: W16, op2: W16
   * @return Map[String, Any]
   */
  def execute_cmp(op1: W16, op2: W16): Map[String, Any] = {
    var resultados = execute_operacion_matematica(_ - _, op1, op2)
    val carryOverflow = takeFlagsRest(resultados("resultado").asInstanceOf[W16], resultados("valor").asInstanceOf[Int], op1, op2)
    guardarResultadosCarryOverflow(resultados, carryOverflow)
  }

  /**
   * Simula la ejecucion del MUL con dos W16 y actualiza los flags, separando el resultado en dos.
   * @params op1: W16, op2: W16
   * @return Map[String, Any]
   */
  def execute_operacion_mul(operacion: (Int, Int) ⇒ Int, op1: W16, op2: W16): Map[String, Any] = {

    val valor = operacion(op1.value, op2.value)
    val resultado_binario = Util.toBinary32B(valor)
    val flags = takeFlags(valor)
    var resultadoMenosSignificativo = new W16(Util.fromBinaryToHex4(resultado_binario._2))
    var resultadoMasSignificativo = new W16(Util.fromBinaryToHex4(resultado_binario._1))

    Map(("R7", resultadoMasSignificativo), ("resultado", resultadoMenosSignificativo), ("n", flags._1), ("z", flags._2), ("c", 0), ("v", 0))

  }
  def guardarResultadosCarryOverflow(resultados: Map[String, Any], map: (Int, Int)): Map[String, Any] = {
    resultados("c") = map._1.asInstanceOf[Any]
    resultados("v") = map._2.asInstanceOf[Any]
    resultados
  }
  /**
   * Devuelve el valor del flag Negative segun el entero que recibe.
   * @params resultado: Int
   * @return Int
   */
  def actualizarNegative(resultado: Int): Int = resultado match {
    case r if (r < 0) ⇒ 1
    case _ ⇒ 0
  }

  /**
   * Devuelve el valor del flag Zero segun el entero que recibe.
   * @params resultado: Int
   * @return Int
   */
  def actualizarZero(resultado: Int): Int = resultado match {
    case r if (r == 0) ⇒ 1
    case _ ⇒ 0
  }

  /**
   * Devuelve el valor del flag Borrow segun el entero que recibe.
   * @params valor: Int
   * @return Int
   */
  def actualizarBorrow(valor: Int): Int = valor match {
    case r if (r < 0) ⇒ 1
    case _ ⇒ 0
  }
  /**
   * Devuelve el valor del flag Carry segun el entero que recibe.
   * @params valor: Int
   * @return Int
   */
  def actualizarCarry(resultado_binario: String): Int = Integer.parseInt(resultado_binario.charAt(0).toString)


  /**
   * Obtiene los bits para analizar Overflow.
   * @params resultado_binario: W16, op1: W16, op2: W16
   * @return (Int, Int, Int)
   */
  def obtenerBitsParaAnalizarOverflow(resultado_binario: W16, op1: W16, op2: W16): (Int, Int, Int) = {
    val bit_significativo_resultado = Integer.parseInt(resultado_binario.toBinary.charAt(0).toString)
    val bit_significativo_op1 = Integer.parseInt(op1.toBinary.charAt(0).toString)
    val bit_significativo_op2 = Integer.parseInt(op2.toBinary.charAt(0).toString)
    (bit_significativo_op1, bit_significativo_op2, bit_significativo_resultado)
  }

  /**
   * Verifica la condicion de overflow en la suma segun los perandos que recibe y el resultado binario.
   * @params resultado_binario: W16, op1: W16, op2: W16
   * @return Int
   */
  def verificarCondicionOverflowSuma(resultado_binario: W16, op1: W16, op2: W16): Int = {
    var bits = obtenerBitsParaAnalizarOverflow(resultado_binario, op1, op2)
    if ((bits._1 == bits._2) && (bits._2 != bits._3)) { 1 } else { 0 }
  }

  /**
   * Verifica la condicion de overflow en la resta segun los perandos que recibe y el resultado binario.
   * @params resultado_binario: W16, op1: W16, op2: W16
   * @return Int
   */
  def verificarCondicionOverflowResta(resultado_binario: W16, op1: W16, op2: W16): Int = {
    var bits = obtenerBitsParaAnalizarOverflow(resultado_binario, op1, op2)
    if ((bits._1 != bits._2) && (bits._2 == bits._3)) { 1 } else { 0 }
  }

  /**
   * Aplica una operacion booleana pasada como funcion por paramentro a dos W16
   * @params op1: W16, op2: W16, operacion: (Int, Int) => Int
   * @return W16
   */
  def aplicarOperacionBooleana(op1: W16, op2: W16, operacion: (Int, Int) ⇒ Int): W16 =
    {
      val una_cadena = op1.toBinary
      val otra_cadena = op2.toBinary
      var result = ""
      var n = 0
      do {
        val bit_a = (una_cadena.charAt(n).toString).toInt
        val bit_b = (otra_cadena.charAt(n).toString).toInt
        result = result + operacion(bit_a, bit_b).toString

        n = n + 1
      } while (n < otra_cadena.size && n < una_cadena.size)

      new W16(Util.binary16ToHex(result))

    }
  def actualizarFlagsOperacionesLogicas(resultado: W16): Map[String, Any] = {
    var valor = resultado.value
    var n = actualizarNegative(valor)
    var z = actualizarZero(valor)
    Map(("resultado", resultado), ("n", n), ("z", z), ("c", 0), ("v", 0))
  }

  /**
   * Simula la ejecucion de AND a dos W16 pasados por parametro
   * @params op1: W16, op2: W16
   * @return W16
   */
  def AND(op1: W16, op2: W16): Map[String, Any] = actualizarFlagsOperacionesLogicas(aplicarOperacionBooleana(op1, op2, AND(_, _)))

  /**
   * Simula la ejecucion de XOR a dos W16 pasados por parametro
   * @params op1: W16, op2: W16
   * @return W16
   */
  def XOR(op1: W16, op2: W16): Map[String, Any] = actualizarFlagsOperacionesLogicas(aplicarOperacionBooleana(op1, op2, XOR(_, _)))

  /**
   * Simula la ejecucion de OR a dos W16 pasados por parametro
   * @params op1: W16, op2: W16
   * @return W16
   */
  def OR(op1: W16, op2: W16): Map[String, Any] = actualizarFlagsOperacionesLogicas(aplicarOperacionBooleana(op1, op2, OR(_, _)))

  /**
   * Simula la ejecucion de NOT a un W16 pasado por parametro
   * @params op1: W16
   * @return W16
   */
  def NOT(op: W16): W16 = {
    var result = ""
    val una_cadena = op.toBinary
    var n = 0
    do {
      val bit_a = (una_cadena.charAt(n).toString).toInt
      result = result + NOT(bit_a).toString

      n = n + 1
    } while (n < una_cadena.size)

    new W16(Util.binary16ToHex(result))

  }

  /**
   * Realiza el AND de dos bits (Int) y devuelve el resultado en entero
   * @params un_bit: Int, otro_bit: Int
   * @return Int
   */
  def AND(un_bit: Int, otro_bit: Int): Int =
    {
      val result = (un_bit, otro_bit) match {
        case (1, 1) ⇒ 1
        case _ ⇒ 0
      }
      result
    }

  /**
   * Realiza el XOR de dos bits (Int) y devuelve el resultado en entero
   * @params un_bit: Int, otro_bit: Int
   * @return Int
   */
  def XOR(un_bit: Int, otro_bit: Int): Int =
    {
      val result = (un_bit, otro_bit) match {
        case (1, 0) ⇒ 1
        case (0, 1) ⇒ 1
        case _ ⇒ 0
      }
      result
    }

  /**
   * Realiza el OR de dos bits (Int) y devuelve el resultado en entero
   * @params un_bit: Int, otro_bit: Int
   * @return Int
   */
  def OR(un_bit: Int, otro_bit: Int): Int =
    {
      val result = (un_bit, otro_bit) match {
        case (1, _) ⇒ 1
        case (_, 1) ⇒ 1
        case _ ⇒ 0
      }
      result
    }
  /**
   * Realiza el NOT de dos bits (Int) y devuelve el resultado en entero
   * @params un_bit: Int
   * @return Int
   */
  def NOT(un_bit: Int): Int =
    {
      un_bit match {
        case 1 ⇒ 0
        case _ ⇒ 1
      }

    }
  /**
   * Intrepreta un bit pasado por parametro, si recibe un uno devuelve un true
   * de lo contrario devuelve cero.
   * @params un_bit: Int
   * @return Boolean
   */
  def interpretarBit(un_bit: Int): Boolean =
    {
      un_bit match {
        case 1 ⇒ true
        case _ ⇒ false
      }

    }
}

object ttaa extends App {
  println(ALU.execute_add(new W16("E000"), new W16("F000")))
  
  //  var sim = Simulador(programa)
  // sim.inicializarSim()
  // sim.cargarPrograma("0003")
}
