package com.github.dleclere.hasp.gridopt.genetics

import com.github.dleclere.hasp.genetics
import com.github.dleclere.hasp.gridopt.{TaskBuilder, TaskId, simpleTaskGraph}
import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.generator._
import com.github.dleclere.hasp.gridopt.genetics.ops.VerificationOps._
import com.github.dleclere.hasp.gridopt.genetics.ops.PrettyPrintOps._

import scala.collection.parallel.immutable.ParVector

object Run extends App {


  import com.github.dleclere.hasp.gridopt.Period
  import com.github.dleclere.hasp.gridopt.{ExecRatePerPeriod, Resource, ResourceId, Task, TaskSuccessor, WorkAmount, WorkTypeTag}
  import com.github.dleclere.hasp.gridopt.genetics.GeneticContext
  import com.github.dleclere.hasp.genetics.BaseImps._
  import com.github.dleclere.hasp.gridopt.genetics._
  import com.github.dleclere.hasp.genetics.base._
  import com.github.dleclere.hasp.genetics.BaseImps._
  import com.github.dleclere.hasp.genetics.random.RandomNumberProvider
  import breeze.stats._
  import breeze.math._
  import breeze.numerics._
  import breeze.stats.distributions._

  import scala.annotation.tailrec
  import scala.math.{exp, log}


//  object sparse_1000_Int extends RandomGraph.IntFactory {
//    val order = 1000
//    val nodeDegrees = NodeDegreeRange(1,10)
//    override def connected = false
//  }
//  val randomSparse = RandomGraph[Int,UnDiEdge,Graph](
//    Graph, sparse_1000_Int, Set(UnDiEdge))
//  val sparseGraph = randomSparse.draw // Graph[Int,UnDiEdge]

  import com.github.dleclere.hasp.gridopt.TaskBuilder._
  val tasks: Seq[Task] = TaskBuilder(
    buildTask(1, 2)(
      TaskSuccessor(2, 4),
      TaskSuccessor(5, 1),
      TaskSuccessor(7, 10),
      TaskSuccessor(4, 1),
      TaskSuccessor(3, 1)
    ),
    buildTask(2, 3)(
      TaskSuccessor(6, 1),
      TaskSuccessor(7, 1)
    ),
    buildTask(5, 5)(),
    buildTask(7, 4)(
      TaskSuccessor(9, 6)
    ),
    buildTask(4, 4)(
      TaskSuccessor(8, 1)
    ),
    buildTask(3, 3)(
      TaskSuccessor(8, 1)
    ),
    buildTask(6, 4)(
      TaskSuccessor(9, 5)
    ),
    buildTask(7, 4)(
      TaskSuccessor(9, 6)
    ),
    buildTask(8, 4)(
      TaskSuccessor(9, 5)
    ),
    buildTask(9, 10)()
  )

//  val tasks: Seq[Task] = TaskBuilder(
//    buildTask(1, 2)(
//      TaskSuccessor(4, 4),
//    ),
//    buildTask(4, 4)(
//      TaskSuccessor(9, 1)
//    ),
//    buildTask(9, 10)()
//  )

  val testContext = new GeneticContext(
    scheduleName = ScheduleName("test"),
    tasks.map(t => t.taskId -> t).toMap,
    resources = Map(
      ResourceId(1) -> Resource(ResourceId(1), ExecRatePerPeriod(Map(WorkTypeTag("default") -> WorkAmount(2)))),
      ResourceId(2) -> Resource(ResourceId(2), ExecRatePerPeriod(Map(WorkTypeTag("default") -> WorkAmount(3)))),
      ResourceId(3) -> Resource(ResourceId(3), ExecRatePerPeriod(Map(WorkTypeTag("default") -> WorkAmount(1)))),
      ResourceId(4) -> Resource(ResourceId(4), ExecRatePerPeriod(Map(WorkTypeTag("default") -> WorkAmount(2)))),
      ResourceId(5) -> Resource(ResourceId(5), ExecRatePerPeriod(Map(WorkTypeTag("default") -> WorkAmount(1))))

    ),
    300
  )

//  val result = testContext.Generator.apply()

//  def crossover[Chromosome[_], Gene](src: Chromosome[Gene], dest: Chromosome[Gene], targetIndex: Int, mutator: CrossoverMutation[Chromosome, Gene])(implicit utils: ChromosomeUtils[Chromosome], gen: RandomNumberProvider): Option[Chromosome[Gene]] =
//  prettyPrint(testContext.graph, result.get)
//
//  println("Schedule verification result: " + verifySchedule(testContext.graph, result.get))
//
//  println(s"RESULT: \n\t$result")
//
//  val mutatedResult = MutateScheduledTask.randomMutate(result.get, 0)
//
//  prettyPrint(testContext.graph, mutatedResult.get)
//
//  println("Mutated Schedule verification result: " + verifySchedule(testContext.graph, mutatedResult.get))
//
//  println(s"RESULT: \n\t$mutatedResult")

//  val result2 = testContext.Generator.apply()
//
//  val result3 = result2.flatMap {
//    _.genes
//      .zipWithIndex
//      .collectFirst {
//        case (gene, index) if result.exists(r => r.genes(index) != gene) =>
//          (gene, index)
//      }
//  }.flatMap {
//    case (gene, index) =>
//      println(s"Applying crossover for $gene")
//      result.flatMap(genetics.crossover(result2.get, _, index, CrossOverScheduledTask))
//  }
//
//  {
//    val Seq(s1, s2) = Seq(result, result3)
//      .map(_.map(_.genes.sortBy(_.start.num)))
//
//    s1.get.zip(s2.get)
//      .filter(tpl => tpl._1 != tpl._2)
//      .foreach {
//        case (a, b) =>
//
//          println(s"DIFF $a \t\t $b")
//
//      }
//  }


//  prettyPrint(testContext.graph, result3.get)
//
//  println("Crossover Schedule verification result: " + verifySchedule(testContext.graph, result3.get))
//
//  println(s"RESULT: \n\t$result3")

  import testContext.CCFMGenerator

  val runResult = genetics.run[Vector, Vector, Vector, ScheduleLike, ScheduledTaskLike[TaskId]](
    Vector(MutateScheduledTask),
    Vector(CrossoverScheduledTask),
    initialPopulationSize = 2,
    populationLimit = 200000,
    cullRate = 0.7f,
    cullPopulationThreshold = 200,
    maxGenerations = 8000,
    crossoverRate = 0.8,
    mutationRate = 0.8
  )

  val graph = simpleTaskGraph(testContext.tasks.values.toSeq: _*)

  val layers = graph.topologicalSort.map(_.toLayered).right.get

  try {
    println("ALL good? " + runResult.forall(verifySchedule(graph, _)))
  } catch {

    case e: Throwable =>
      e.printStackTrace()

  }

  println("RUN RESULT: ")
  runResult.seq.sortBy(_.genes.maxBy(_.end.num).end.num).take(5)
    .foreach { result =>
      println("\t\t" + result.genes.maxBy(_.end.num).end)
    }

  prettyPrint(graph, runResult.head)
//
//  val examples = scala.collection.immutable.Vector(
//    (1d, 0.50d),
//    (2d, 0.30d),
//    (3d, 0.15d),
//    (4d, 0.05d)
//  )
//
//  val sampleSize = 10000
//  val esamples: Seq[Double] = (0 to sampleSize).map(_ => monteCarlo(examples))
//  def popGen(inputs: scala.collection.immutable.Vector[(Double, Double)], popSize: Int): scala.collection.immutable.Vector[Double] =
//    inputs.flatMap { case (a, pct) =>
//      scala.collection.immutable.Vector.fill(Math.round(popSize.toDouble * pct).toInt)(a)
//    }
//  val pop = popGen(examples, 100000)
//  println("MEAN AND VARIANCE: " + meanAndVariance(esamples) + ", actual: " + meanAndVariance(pop))
//  println("STANDARD DEVIATION: " + stddev(esamples) + ", actual: " + stddev(pop))
//
//  val rate = 1
//  val expo = new Exponential(rate)
//  val chai = new ChiSquared(2)
//  chai.probability(0, 1)
//  //chai.sample()
//  val samples = chai.sample(1000)
//  println("MEAN AND VARIANCE: " + meanAndVariance(samples))
//  println("STANDARD DEVIATION: " + stddev(samples))
//
//  @tailrec
//  final def periodSelector[S[_], A](weighted: S[(A, Double)])(implicit gen: RandomNumberProvider, utils: CoreCollectionUtils[S]): A =
//    utils(weighted)(gen.random(utils.length(weighted))) match {
//      case (s, f) if f > gen.nextFloat => s
//      case _ => periodSelector(weighted)
//    }
//
//  chai.cdf(3d)
  //def max(rate: Double): Double = Math.log(rate) / rate

  //expo.




  def chooseResourcePeriod2(windowStart: Period, windowEnd: Period, duration: Period, dist: Rand[Double] with HasCdf, slackFactor: Int = 3, earlyBlockBiasDeflator: Double = 0.3, earlyPeriodBiasDeflator: Double = 0.1)(implicit rand: RandomNumberProvider): (Period, Period) = {

    def bias(selectionSize: Double, selectionPossibilityRange: Double, deflator: Double): Double =
      Math.max((1d - (selectionSize.toDouble / selectionPossibilityRange)) * deflator, selectionSize / selectionPossibilityRange)

    val latestStart = windowEnd.num - duration.num
    val possibilityRange = (latestStart  - windowStart.num).toDouble
    val periodBlockSize = duration.num * slackFactor
    val periodBlockCount = (possibilityRange / periodBlockSize.toDouble).toInt
    val blockBias = bias(periodBlockSize.toDouble, possibilityRange, earlyBlockBiasDeflator)
    val periodBias = bias(1d, periodBlockSize.toDouble, earlyPeriodBiasDeflator)

    val chosenPeriod = biasedSelection(0 to periodBlockCount, blockBias)
    val chosenStart = chosenPeriod * periodBlockSize + biasedSelection(0 to periodBlockSize, periodBias)
    (Period(chosenStart), Period(chosenStart + duration.num))
  }

  def biasedSelection[C[_], T](choices: C[T], earlyBias: Double)(implicit utils: CollectionUtils[C]): T  =
    monteCarlo {
      utils.foldLeft(choices)((1d, utils.empty[(T, Double)])) {
        case ((rem, bs), index) =>
          val pct = rem * earlyBias
          (rem - pct, utils.append(bs)(index -> pct))
      }._2
    }
//
//  val starts = (0 to 10000).map { _ =>
//
//    val (start, _) = chooseResourcePeriod2(Period(0), Period(10000), Period(100), new Exponential(1), slackFactor = 1, earlyBlockBiasDeflator = 0.5d)
//    start.num.toDouble
//
//  }
//
//  println("MEAN AND VARIANCE: " + meanAndVariance(starts))
//  println("STANDARD DEVIATION: " + stddev(starts))
//  println("MIN:  " + starts.min)
//  println("MAX:  " + starts.max)

//  import scala.math._
//  def expRandom(rate: Double, min: Double, max: Double)(implicit gen: RandomNumberProvider): Double = {
//    def e(x: Double) = exp(-rate * x)
//    -log(e(min) - gen.nextDouble * (e(min) - e(max)))/rate
//  }
//
//  def sround(num: Double, p: Int = 3): Double =
//    round(num * pow(10, 3))/pow(10,3)
//
//  (1 to 200).map { rate =>
//    val erands: Seq[Double] = (0 to 10000).map(_ => expRandom(2, 0, 1000))
//    val mv = meanAndVariance(erands)
//    val std  = stddev(erands)
//    (rate, sround(mv.mean), sround(mv.variance), sround(std), erands.min, erands.max)
//  }.sortBy(_._2)
//    .foreach { case (r, mean, variance, std, min, max) =>
//      println(s"rate: $r, mean: $mean, var: $variance, std: $std, min: $min, max: $max")
//    }


  //testContext.Generator.apply()

  //testContext.Generator.apply()
  //testContext.Generator.apply()
  //testContext.Generator.apply()

}
