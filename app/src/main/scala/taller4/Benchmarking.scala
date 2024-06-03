package taller4

import org.scalameter._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration._
import taller4.Newton.{Div => DivN, Expo => ExpoN, Numero => NumeroN, Prod => ProdN, Resta => RestaN, Suma => SumaN, Atomo => AtomoN, Logaritmo => LogaritmoN}
import taller4.NewtonParalelo.{Div => DivNP, Expo => ExpoNP, Numero => NumeroNP, Prod => ProdNP, Resta => RestaNP, Suma => SumaNP, Atomo => AtomoNP, Logaritmo => LogaritmoNP}
object Benchmarking {

  val largeNumberExprN = ProdN(NumeroN(1000), ExpoN(AtomoN('x'), NumeroN(3)))
  val largeNumberExprNP = ProdNP(NumeroNP(1000), ExpoNP(AtomoNP('x'), NumeroNP(3)))
  val initialGuess = 10.0
  val tolerance = 1e-7
  val maxIterations = 1000
  val variableN = AtomoN('x')
  val variableNP = AtomoNP('x')

  def benchmarkNewton(): Unit = {
    val time = withWarmer(new Warmer.Default) measure {
      Newton.raizNewton(largeNumberExprN, variableN, initialGuess, tolerance, maxIterations)
    }
    println(s"tiempo de ejecucion: $time")
  }

  def benchmarkNewtonParalelo(): Unit = {
    val time = withWarmer(new Warmer.Default) measure {
      val futureResult = NewtonParalelo.raizNewton(largeNumberExprNP, variableNP, initialGuess, tolerance, maxIterations)
      Await.result(futureResult, Duration.Inf)
    }
    println(s"tiempo de ejecucion: $time")
  }

  def runBenchmarks(): Unit = {
    println("Benchmarking de Newton de forma secuencial")
    benchmarkNewton()
    println("Benchmarking Newton de forma paralela")
    benchmarkNewtonParalelo()
  }
}