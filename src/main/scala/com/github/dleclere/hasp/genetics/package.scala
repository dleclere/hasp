package com.github.dleclere.hasp

import com.github.dleclere.hasp.genetics.random.RandomNumberProvider

import scala.annotation.tailrec
import scala.language.higherKinds
import scala.math.Ordering
import scala.util.Random

package object genetics {
  import base._

  def mutate[MutationProvider[_], Chromosome[_], Gene](c: Chromosome[Gene], rate: Double, mutators: => MutationProvider[RandomMutation[Chromosome, Gene]])(implicit gen: RandomNumberProvider, utils: ChromosomeUtils[Chromosome], pUtils: CollectionUtils[MutationProvider]): Option[Chromosome[Gene]] = {
    val rounds = Math.ceil(utils.length(c).toDouble * rate).toInt
    val cLen = utils.length(c)
    if (pUtils.length(mutators) > 0)
      (0 until rounds)
        .map(_ => (c: Chromosome[Gene]) =>
          applyTillSome(random(mutators)(gen, pUtils).randomMutate(c, gen.random(cLen)))
        )
        .foldLeft(Option(c))(_ map _)
    else
      None
  }

  def select[Population[_], Chromosome[_], Gene](p: Population[Chromosome[Gene]])(implicit fitter: Fitter[Chromosome, Gene], utils: CollectionUtils[Population], cUtils: ChromosomeUtils[Chromosome]): Population[Chromosome[Gene]] = {
    import utils._
    map(sortBy(map(p)(c => c -> fitter.fitness(c)))(_._2))(_._1)
  }

  def crossover[FactoryProvider[_], Chromosome[_], Gene](left: Chromosome[Gene], right: Chromosome[Gene], rate: Double, performers: FactoryProvider[CrossoverMutation[Chromosome, Gene]])(implicit gen: RandomNumberProvider, utils: ChromosomeUtils[Chromosome], fUtils: CollectionUtils[FactoryProvider]): Seq[Chromosome[Gene]] = {
    val shortestChromosome = Math.min(utils.length(left), utils.length(right))
    (0 until Math.round(shortestChromosome.toDouble * rate).toInt)
      .foldLeft(Seq(left -> right, right -> left)) { (prs, _) =>
        def index = gen.random(shortestChromosome)
        def mutation = random(performers)
        val i = gen.random(2)
        val tpl@(src, dest) = prs(i)
        prs.updated(i, tpl.copy(_2 = applyTillSome(crossover(src, dest, index, mutation))))
      }.map(_._2)//.foldLeft(pUtils.empty[Chromosome[Gene]])(pUtils.append(_)(_))
  }

  def crossover[Chromosome[_], Gene](src: Chromosome[Gene], dest: Chromosome[Gene], targetIndex: Int, mutator: CrossoverMutation[Chromosome, Gene])(implicit utils: ChromosomeUtils[Chromosome], gen: RandomNumberProvider): Option[Chromosome[Gene]] =
    Option(dest).flatMap(mutator.crossoverMutate(utils(src)(targetIndex), _, targetIndex))

  def run[MutationProvider[_], CrossoverProvider[_], Population[_], Chromosome[_], Gene](
    mutators: MutationProvider[RandomMutation[Chromosome, Gene]],
    crossovers: CrossoverProvider[CrossoverMutation[Chromosome, Gene]],
    initialPopulationSize: Int,
    populationLimit: Int,
    cullRate: Float,
    cullPopulationThreshold: Int,
    maxGenerations: Int,
    crossoverRate: Double,
    mutationRate: Double
  )(implicit fitter: Fitter[Chromosome, Gene], mu: CollectionUtils[MutationProvider], cou: CollectionUtils[CrossoverProvider], pu: CollectionUtils[Population], cu: ChromosomeUtils[Chromosome], rand: RandomNumberProvider, gen: GenChromosome[Chromosome, Gene]): Population[Chromosome[Gene]] = {

    def fitness(mate: Chromosome[Gene]): (Chromosome[Gene], Double) =
      mate -> fitter.fitness(mate)

    def normalisedFitness(mate: (Chromosome[Gene], Double), sumFitness: Double): (Chromosome[Gene], Double) =
      mate.copy(_2 = mate._2 / sumFitness)

    def makeInitialPopulation(): Population[Chromosome[Gene]] = {

      @tailrec
      def inner(generated: Population[Chromosome[Gene]], generateAmount: Int): Population[Chromosome[Gene]] = {
        val newGen = (0 until generateAmount)
          .par
          .flatMap(_ => gen.apply())
        val remaining = generateAmount - newGen.length
        val newTotal = newGen.foldLeft(generated)((m, g) => pu.prepend(m)(g))
        if (remaining <= 0)
          newTotal
        else
          inner(newTotal, remaining)
      }
      inner(pu.empty, initialPopulationSize)
    }

    @tailrec
    def evole(pop: Population[(Chromosome[Gene], Double)], populationCount: Int, generation: Int): Population[Chromosome[Gene]] = {
      import pu._
      println(s"Starting generation $generation, size: ${pu.length(pop)}, fittest: ${pu.toSeq(pu.map(pop)(_._2)).min}")
      val sumFitness = foldLeft(pop)(0d)(_ + _._2)
      val normalMates = map(pop)(normalisedFitness(_, sumFitness))

      val (newPop, newPopSize) =
        foldLeft(pop)((pop, populationCount)) { case ((p, pc), _) =>
          val c1 = monteCarlo(normalMates)
          val c2 = monteCarlo(normalMates)
          crossover(c1, c2, crossoverRate, crossovers)
            .map(c => mutate(c, mutationRate, mutators).getOrElse(c))
            .map(fitness)
            .foldLeft((p, pc)) { case ((popu, _pc), fit) =>
              (pu.prepend(popu)(fit), _pc + 1)
            }
        }

      def sorted =
        map(
          sortBy(map(newPop)(c => c -> fitter.fitness(c._1)))(_._1._2)
        )(_._1)

      val (culledPop, culledPopSize) = {
        def doCull(cullAmount: Int) =
          (take(sorted)(newPopSize - cullAmount), newPopSize - cullAmount)
        if (newPopSize > populationLimit)
          doCull(newPopSize - populationLimit)
        else if (newPopSize > cullPopulationThreshold)
          doCull(newPopSize - (newPopSize.toFloat * (1f - cullRate)).round)
        else
          (newPop, newPopSize)
      }
      if (generation == maxGenerations) //(stopCondition(newGeneration))
        map(newPop)(_._1)
      else {
        evole(culledPop, culledPopSize, generation + 1)
      }
    }
    val initialPopulation = makeInitialPopulation()
    val initialFitness = pu.map(initialPopulation)(fitness)
    select(evole(initialFitness, initialPopulationSize, 0))
  }


}
