package ar.edu.unq.tpi.qsim

import ar.edu.unq.tpi.qsim.model.W16

package object utils {
	
  implicit def stringToW16(string:String) = new W16(string)
}