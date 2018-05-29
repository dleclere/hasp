package com.github.dleclere.hasp.utils

import org.scalatest.{FlatSpec, Matchers}
import VecSync._

import scala.util.Random

class UtilsSpec extends FlatSpec with Matchers  {
  
  def rand(max: Int): Int =
    if (max <= 0)
      0
    else
      Random.nextInt(max)


  "A seq with random adds, removes, and shuffled" should "sync back to the original seq" in {

    (0 to 1000).foreach { _ =>
      val oustart = 20
      val ouend = 40
      val original = (oustart to ouend).toVector
      val toAddStart = rand(oustart - 1)
      val toAdd = (toAddStart until (toAddStart + rand(oustart - toAddStart))).toVector
      val toRemoveStart = oustart + (rand(ouend - 1) - oustart)
      val toRemove =  toRemoveStart until toRemoveStart + rand(ouend - toRemoveStart)
      val next = {
        val p1 = Random.shuffle(original diff toRemove)
        toAdd.foldLeft(p1) { (m, a) =>
          insert(m, rand(m.length - 1), a)
        }
      }
      applySyncOps(original, syncOps(original, next)) should be (next)
    }

  }
}
