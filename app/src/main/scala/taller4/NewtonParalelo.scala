/**
 * Taller 4 - Programación Funcional
 * Autores: Kevin Alexis lorza y Juan David Perez
 * Profesor: Carlos A Delgado
 */
package taller4

import scala.concurrent.{Future, ExecutionContext}
import scala.util.{Failure, Success}
import scala.concurrent.Await
import scala.concurrent.duration._

object NewtonParalelo {

  trait Expr

  case class Numero(d: Double) extends Expr

  case class Atomo(x: Char) extends Expr

  case class Suma(e1: Expr, e2: Expr) extends Expr

  case class Prod(e1: Expr, e2: Expr) extends Expr

  case class Resta(e1: Expr, e2: Expr) extends Expr

  case class Div(e1: Expr, e2: Expr) extends Expr

  case class Expo(e1: Expr, e2: Expr) extends Expr

  case class Logaritmo(e1: Expr) extends Expr

  def mostrar(e: Expr): String = e match {
    case Numero(n) => n.toString
    case Atomo(x) => x.toString
    case Suma(e1, e2) => "(" + mostrar(e1) + "+" + mostrar(e2) + ")"
    case Prod(e1, e2) => "(" + mostrar(e1) + "*" + mostrar(e2) + ")"
    case Resta(e1, e2) => "(" + mostrar(e1) + "-" + mostrar(e2) + ")"
    case Div(e1, e2) => "(" + mostrar(e1) + "/" + mostrar(e2) + ")"
    case Expo(e1, e2) => "(" + mostrar(e1) + "^" + mostrar(e2) + ")"
    case Logaritmo(e1) => "(" + "log(" + mostrar(e1) + ")" + ")"
  }

  def derivar(e: Expr, a: Atomo): Expr = e match {
    case Numero(_) => Numero(0)
    case Atomo(x) => if (x == a.x) Numero(1) else Numero(0)
    case Suma(e1, e2) => Suma(derivar(e1, a), derivar(e2, a))
    case Prod(e1, e2) => Suma(Prod(derivar(e1, a), e2), Prod(e1, derivar(e2, a)))
    case Resta(e1, e2) => Resta(derivar(e1, a), derivar(e2, a))
    case Div(e1, e2) => Div(Resta(Prod(derivar(e1, a), e2), Prod(e1, derivar(e2, a))), Prod(e2, e2))
    case Expo(e1, e2) => Prod(Prod(e2, Expo(e1, Resta(e2, Numero(1)))), derivar(e1, a))
    case Logaritmo(e1) => Div(derivar(e1, a), e1)
  }

  def evaluar(e: Expr, a: Atomo, v: Double): Double = e match {
    case Numero(n) => n
    case Atomo(x) => if (x == a.x) v else 0
    case Suma(e1, e2) => evaluar(e1, a, v) + evaluar(e2, a, v)
    case Prod(e1, e2) => evaluar(e1, a, v) * evaluar(e2, a, v)
    case Resta(e1, e2) => evaluar(e1, a, v) - evaluar(e2, a, v)
    case Div(e1, e2) => evaluar(e1, a, v) / evaluar(e2, a, v)
    case Expo(e1, e2) => Math.pow(evaluar(e1, a, v), evaluar(e2, a, v))
    case Logaritmo(e1) => Math.log(evaluar(e1, a, v))
  }

  def limpiar(e: Expr): Expr = e match {
    case Numero(n) => Numero(n)
    case Atomo(x) => Atomo(x)
    case Suma(e1, e2) => {
      val e1L = limpiar(e1)
      val e2L = limpiar(e2)
      (e1L, e2L) match {
        case (Numero(0), _) => e2L
        case (_, Numero(0)) => e1L
        case (Numero(n1), Numero(n2)) => Numero(n1 + n2)
        case _ => Suma(e1L, e2L)
      }
    }
    case Prod(e1, e2) => {
      val e1L = limpiar(e1)
      val e2L = limpiar(e2)
      (e1L, e2L) match {
        case (Numero(0), _) => Numero(0)
        case (_, Numero(0)) => Numero(0)
        case (Numero(1), _) => e2L
        case (_, Numero(1)) => e1L
        case (Numero(n1), Numero(n2)) => Numero(n1 * n2)
        case _ => Prod(e1L, e2L)
      }
    }
    case Resta(e1, e2) => {
      val e1L = limpiar(e1)
      val e2L = limpiar(e2)
      (e1L, e2L) match {
        case (Numero(0), _) => Prod(Numero(-1), e2L)
        case (_, Numero(0)) => e1L
        case (Numero(n1), Numero(n2)) => Numero(n1 - n2)
        case _ => Resta(e1L, e2L)
      }
    }
    case Div(e1, e2) => {
      val e1L = limpiar(e1)
      val e2L = limpiar(e2)
      (e1L, e2L) match {
        case (Numero(0), _) => Numero(0)
        case (_, Numero(0)) => throw new IllegalArgumentException("Division por cero")
        case (Numero(n1), Numero(n2)) => Numero(n1 / n2)
        case _ => Div(e1L, e2L)
      }
    }
    case Expo(e1, e2) => {
      val e1L = limpiar(e1)
      val e2L = limpiar(e2)
      (e1L, e2L) match {
        case (_, Numero(0)) => Numero(1)
        case (_, Numero(1)) => e1L
        case (Numero(0), _) => Numero(0)
        case (Numero(1), _) => Numero(1)
        case (Numero(n1), Numero(n2)) => Numero(Math.pow(n1, n2))
        case _ => Expo(e1L, e2L)
      }
    }
    case Logaritmo(e1) => {
      val e1L = limpiar(e1)
      e1L match {
        case Numero(1) => Numero(0)
        case _ => Logaritmo(e1L)
      }
    }
  }

  def raizNewton(e: Expr, a: Atomo, x0: Double, tol: Double = 1e-7, maxIter: Int = 1000)(implicit ec: ExecutionContext): Future[Double] = {
    def isCloseEnough(value: Double): Boolean = Math.abs(value) < tol

    def newtonIter(x: Double, iter: Int): Future[Double] = Future {
      if (iter >= maxIter)
        throw new ArithmeticException("No se encontró solución")
      else {
        val fValue = evaluar(e, a, x)
        if (isCloseEnough(fValue)) x
        else {
          val fPrime = evaluar(derivar(e, a), a, x)
          val newX = x - fValue / fPrime
          Await.result(newtonIter(newX, iter + 1), Duration.Inf)
        }
      }
    }

    newtonIter(x0, 0)
  }
}