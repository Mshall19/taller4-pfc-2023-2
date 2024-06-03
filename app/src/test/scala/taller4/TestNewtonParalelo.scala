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
import org.scalatest.concurrent.ScalaFutures._
import org.scalatest.matchers.should.Matchers._
import taller4.NewtonParalelo.{Atomo, Div, Expo, Logaritmo, Numero, Prod, Resta, Suma}

import scala.concurrent.ExecutionContext.Implicits.global

@RunWith(classOf[JUnitRunner])
class TestNewtonParalelo extends AnyFunSuite{

    /*test de mostrar paralelo*/

    test("test suma "){
      val e = Suma(Suma(Numero(24), Atomo('x')), Numero(10))
      assert(NewtonParalelo.mostrar(e) == "((24.0+x)+10.0)")
    }
    test("test multiplicacion "){
      val e = Prod(Prod(Numero(2), Numero(1945)), Atomo('x'))
      assert(NewtonParalelo.mostrar(e) == "((2.0*1945.0)*x)")
    }
    test("test resta paralelo"){
      val e = Resta(Resta(Atomo('x'), Numero(2)), Numero(5))
      assert(NewtonParalelo.mostrar(e) == "((x-2.0)-5.0)")
    }
    test("test division paralelo"){
      val e = Div(Div(Numero(3.7), Numero(2)), Atomo('x'))
      assert(NewtonParalelo.mostrar(e) == "((3.7/2.0)/x)")
    }
    test("test exponenciacion paralelo"){
      val e = Expo(Expo(Numero(87), Atomo('x')), Numero(2))
      assert(NewtonParalelo.mostrar(e) == "((87.0^x)^2.0)")
    }
    test("test logaritmo paralelo"){
      val e = Logaritmo(Logaritmo(Logaritmo(Numero(14))))
      assert(NewtonParalelo.mostrar(e) == "(log((log((log(14.0))))))")
    }

    /*test de derivar paralelo*/

    test("test derivar suma paralelo"){
      val e = Suma(Suma(Numero(24), Atomo('x')), Numero(10))
      val a = Atomo('x')
      assert(NewtonParalelo.mostrar(NewtonParalelo.derivar(e, a)) == "((0.0+1.0)+0.0)")
    }
    test("test derivar multiplicacion paralelo"){
      val e = Prod(Prod(Numero(2), Numero(1945)), Atomo('x'))
      val a = Atomo('x')
      assert(NewtonParalelo.mostrar(NewtonParalelo.derivar(e, a)) == "((((0.0*1945.0)+(2.0*0.0))*x)+((2.0*1945.0)*1.0))")
    }
    test("test derivar resta paralelo"){
      val e = Resta(Resta(Atomo('x'), Numero(2)), Numero(5))
      val a = Atomo('x')
      assert(NewtonParalelo.mostrar(NewtonParalelo.derivar(e, a)) == "((1.0-0.0)-0.0)")
    }
    test("test derivar division paralelo"){
      val e = Div(Div(Numero(3.7), Numero(2)), Atomo('x'))
      val a = Atomo('x')
      assert(NewtonParalelo.mostrar(NewtonParalelo.derivar(e, a)) == "((((((0.0*2.0)-(3.7*0.0))/(2.0*2.0))*x)-((3.7/2.0)*1.0))/(x*x))")
    }
    test("test derivar exponenciacion paralelo"){
      val e = Expo(Expo(Numero(87), Atomo('x')), Numero(2))
      val a = Atomo('x')
      assert(NewtonParalelo.mostrar(NewtonParalelo.derivar(e, a)) == "((2.0*((87.0^x)^(2.0-1.0)))*((x*(87.0^(x-1.0)))*0.0))")
    }
    test("test derivar logaritmo paralelo"){
      val e = Logaritmo(Logaritmo(Logaritmo(Numero(14))))
      val a = Atomo('x')
      assert(NewtonParalelo.mostrar(NewtonParalelo.derivar(e, a)) == "(((0.0/14.0)/(log(14.0)))/(log((log(14.0)))))")
    }

    /*test de evaluar paralelo*/

    test("test evaluar suma paralelo"){
      val e = Suma(Suma(Numero(24), Atomo('x')), Numero(10))
      val a = Atomo('x')
      val v = 5
      assert(NewtonParalelo.evaluar(e, a, v) == 39)
    }
    test("test evaluar multiplicacion paralelo"){
      val e = Prod(Prod(Numero(2), Numero(1945)), Atomo('x'))
      val a = Atomo('x')
      val v = 3
      assert(NewtonParalelo.evaluar(e, a, v) == 11670)
    }
    test("test evaluar resta paralelo"){
      val e = Resta(Resta(Atomo('x'), Numero(2)), Numero(5))
      val a = Atomo('x')
      val v = 7
      assert(NewtonParalelo.evaluar(e, a, v) == 0)
    }
    test("test evaluar division paralelo"){
      val e = Div(Div(Numero(3.7), Numero(2)), Atomo('x'))
      val a = Atomo('x')
      val v = 2
      assert(NewtonParalelo.evaluar(e, a, v) == 0.925)
    }
    test("test evaluar exponenciacion paralelo"){
      val e = Expo(Expo(Numero(87), Atomo('x')), Numero(2))
      val a = Atomo('x')
      val v = 3
      assert(NewtonParalelo.evaluar(e, a, v) == 4.33626201009E11)
    }
    test("test evaluar logaritmo paralelo"){
      val e = Logaritmo(Logaritmo(Logaritmo(Numero(14))))
      val a = Atomo('x')
      val v = 2
      assert(NewtonParalelo.evaluar(e, a, v) == -0.030024475935024546)
    }

    /*test de limpiar paralelo*/

    test("test limpiar suma paralelo"){
      val e = Suma(Suma(Numero(0), Atomo('x')), Numero(0))
      assert(NewtonParalelo.mostrar(NewtonParalelo.limpiar(e)) == "x")
    }
    test("test limpiar multiplicacion paralelo"){
      val e = Prod(Prod(Numero(1), Numero(1945)), Atomo('x'))
      assert(NewtonParalelo.mostrar(NewtonParalelo.limpiar(e)) == "(1945.0*x)")
    }
    test("test limpiar resta paralelo"){
      val e = Resta(Resta(Atomo('x'), Numero(0)), Numero(0))
      assert(NewtonParalelo.mostrar(NewtonParalelo.limpiar(e)) == "x")
    }
    test("test limpiar division paralelo"){
      val e = Div(Div(Numero(0), Numero(2)), Atomo('x'))
      assert(NewtonParalelo.mostrar(NewtonParalelo.limpiar(e)) == "0.0")
    }
    test("test limpiar exponenciacion paralelo"){
      val e = Expo(Expo(Numero(87), Atomo('x')), Numero(1))
      assert(NewtonParalelo.mostrar(NewtonParalelo.limpiar(e)) == "(87.0^x)")
    }
    test("test limpiar logaritmo paralelo"){
      val e = Logaritmo(Logaritmo(Logaritmo(Numero(14))))
      assert(NewtonParalelo.mostrar(NewtonParalelo.limpiar(e)) == "(log((log((log(14.0))))))")
    }

  /*test de raiz paralelo*/

  test("test raiz") {
    val e = Suma(Prod(Numero(2), Atomo('x')), Numero(3))
    val a = Atomo('x')
    val v = -1.4
    val futureResult = NewtonParalelo.raizNewton(e, a, v, 0.00001)
    futureResult.futureValue shouldBe -1.5
  }
  test("test raiz x^2 - 4x + 4") {
    val e = Suma(Resta(Prod(Numero(1), Expo(Atomo('x'), Numero(2))), Prod(Numero(4), Atomo('x'))), Numero(4))
    val a = Atomo('x')
    val v = 2.5
    val futureResult = NewtonParalelo.raizNewton(e, a, v, 0.00001)
    futureResult.futureValue shouldBe 2.001953125
  }
  test("test raiz log(x) - 2") {
    val e = Resta(Logaritmo(Atomo('x')), Numero(2))
    val a = Atomo('x')
    val v = 7.5
    val expected = Math.exp(2)
    val futureResult = NewtonParalelo.raizNewton(e, a, v, 0.00001)
    futureResult.futureValue shouldBe expected +- 0.00001
  }
  test("test raiz x^3 - 27") {
    val e = Resta(Expo(Atomo('x'), Numero(3)), Numero(27))
    val a = Atomo('x')
    val v = 3.5
    val futureResult = NewtonParalelo.raizNewton(e, a, v, 0.00001)
    futureResult.futureValue shouldBe 3.000000000000186
  }
  test("test raiz x^2 - 2x - 3") {
    val e = Resta(Suma(Expo(Atomo('x'), Numero(2)), Prod(Numero(-2), Atomo('x'))), Numero(3))
    val a = Atomo('x')
    val v = 3.5
    val futureResult = NewtonParalelo.raizNewton(e, a, v, 0.00001)

    futureResult.futureValue shouldBe 3.0000000929222947
  }






}