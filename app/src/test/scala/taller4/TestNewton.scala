/**
 * Plantilla para pruebas
 * @author Carlos Delgado
 * @version 1.0
 * @note 22 de Noviembre de 2023
 */
package taller4

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import taller4.Newton.{Div, Expo, Numero, Prod, Resta, Suma, Atomo, Logaritmo}

@RunWith(classOf[JUnitRunner])
class TestNewton extends AnyFunSuite{

  /*test de mostrar*/

  test("test suma"){
    val e = Suma(Numero(24), Atomo('x'))
    assert(Newton.mostrar(e) == "(24.0+x)")
  }
  test("test multiplicacion"){
    val e = Prod(Numero(2), Numero(1945))
    assert(Newton.mostrar(e) == "(2.0*1945.0)")
  }
  test("test resta"){
    val e = Resta(Atomo('x'), Numero(2))
    assert(Newton.mostrar(e) == "(x-2.0)")
  }
  test("test division"){
    val e = Div(Numero(3.7), Numero(2))
    assert(Newton.mostrar(e) == "(3.7/2.0)")
  }
  test("test exponenciacion"){
    val e = Expo(Numero(87), Atomo('x'))
    assert(Newton.mostrar(e) == "(87.0^x)")
  }
  test("test logaritmo"){
    val e = Logaritmo(Numero(13))
    assert(Newton.mostrar(e) == "(log(13.0))")
  }


  /*test de derivar*/

  test("test derivar suma"){
    val e = Suma(Numero(24), Atomo('x'))
    val a = Atomo('x')
    assert(Newton.mostrar(Newton.derivar(e, a)) == "(0.0+1.0)")
  }
  test("test derivar multiplicacion"){
    val e = Prod(Numero(2), Numero(1945))
    val a = Atomo('x')
    assert(Newton.mostrar(Newton.derivar(e, a)) == "((0.0*1945.0)+(2.0*0.0))")
  }
  test("test derivar resta"){
    val e = Resta(Atomo('x'), Numero(2))
    val a = Atomo('x')
    assert(Newton.mostrar(Newton.derivar(e, a)) == "(1.0-0.0)")
  }
  test("test derivar division"){
    val e = Div(Numero(3.7), Numero(2))
    val a = Atomo('x')
    assert(Newton.mostrar(Newton.derivar(e, a)) == "(((0.0*2.0)-(3.7*0.0))/(2.0*2.0))")
  }
  test("test derivar exponenciacion"){
    val e = Expo(Numero(87), Atomo('x'))
    val a = Atomo('x')
    assert(Newton.mostrar(Newton.derivar(e, a)) == "((x*(87.0^(x-1.0)))*0.0)")
  }
  test("test derivar logaritmo"){
    val e = Logaritmo(Numero(13))
    val a = Atomo('x')
    assert(Newton.mostrar(Newton.derivar(e, a)) == "(0.0/13.0)")
  }


  /*test de evaluar*/

  test("test evaluar suma"){
    val e = Suma(Numero(24), Atomo('x'))
    val a = Atomo('x')
    val v = 3
    assert(Newton.evaluar(e, a, v) == 27)
  }
  test("test evaluar multiplicacion"){
    val e = Prod(Numero(2), Numero(1945))
    val a = Atomo('x')
    val v = 3
    assert(Newton.evaluar(e, a, v) == 3890)
  }
  test("test evaluar resta"){
    val e = Resta(Atomo('x'), Numero(2))
    val a = Atomo('x')
    val v = 3
    assert(Newton.evaluar(e, a, v) == 1)
  }
  test("test evaluar division"){
    val e = Div(Numero(3.7), Numero(2))
    val a = Atomo('x')
    val v = 3
    assert(Newton.evaluar(e, a, v) == 1.85)
  }
  test("test evaluar exponenciacion"){
    val e = Expo(Numero(87), Atomo('x'))
    val a = Atomo('x')
    val v = 3
    assert(Newton.evaluar(e, a, v) == 658503.0)
  }
  test("test evaluar logaritmo"){
    val e = Logaritmo(Numero(13))
    val a = Atomo('x')
    val v = 3
    assert(Newton.evaluar(e, a, v) == 2.5649493574615367)
  }

  /*test de limpiar*/

  test("test limpiar suma"){
    val e = Suma(Numero(0), Numero(2))
    assert(Newton.mostrar(Newton.limpiar(e)) == "2.0")
  }
  test("test limpiar multiplicacion"){
    val e = Prod(Numero(1), Numero(2))
    assert(Newton.mostrar(Newton.limpiar(e)) == "2.0")
  }
  test("test limpiar resta"){
    val e = Resta(Numero(2), Numero(2))
    assert(Newton.mostrar(Newton.limpiar(e)) == "0.0")
  }
  test("test limpiar division"){
    val e = Div(Numero(2), Numero(2))
    assert(Newton.mostrar(Newton.limpiar(e)) == "1.0")
  }
  test("test limpiar exponenciacion"){
    val e = Expo(Numero(2), Numero(2))
    assert(Newton.mostrar(Newton.limpiar(e)) == "4.0")
  }
  test("test limpiar logaritmo"){
    val e = Logaritmo(Numero(2))
    assert(Newton.mostrar(Newton.limpiar(e)) == "(log(2.0))")
  }

  /*test de raiz*/

  test("test raiz x^2 - 4x + 4") {
    val e = Suma(Resta(Prod(Numero(1), Expo(Atomo('x'), Numero(2))), Prod(Numero(4), Atomo('x'))), Numero(4))
    val a = Atomo('x')
    val v = 2.5
    assert(Newton.raizNewton(e, a, v, 0.00001) == 2.001953125)
  }

  test("test raiz log(x) - 2") {
    val e = Resta(Logaritmo(Atomo('x')), Numero(2))
    val a = Atomo('x')
    val v = 7.5
    assert(Math.abs(Newton.raizNewton(e, a, v, 0.00001) - Math.exp(2)) < 0.00001)
  }

  test("test raiz x^3 - 27") {
    val e = Resta(Expo(Atomo('x'), Numero(3)), Numero(27))
    val a = Atomo('x')
    val v = 3.5
    assert(Newton.raizNewton(e, a, v, 0.00001) == 3.000000000000186)
  }

  test("test raiz x^2 - 2x - 3") {
    val e = Resta(Suma(Expo(Atomo('x'), Numero(2)), Prod(Numero(-2), Atomo('x'))), Numero(3))
    val a = Atomo('x')
    val v = 3.5
    assert(Newton.raizNewton(e, a, v, 0.00001) == 3.0000000929222947)
  }

  test("test raiz 2x + 3"){
    val e = Suma(Prod(Numero(2), Atomo('x')), Numero(3))
    val a = Atomo('x')
    val v = (-1.4)
    assert(Newton.raizNewton(e,a,v,0.00001) == (-1.5))
  }
}

