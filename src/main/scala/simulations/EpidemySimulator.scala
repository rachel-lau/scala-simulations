package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    // to complete: additional parameters of simulation
    val prevalenceRate = 0.01
  }

  import SimConfig._

  val persons: List[Person] = {
    val numInfected = (SimConfig.population * SimConfig.prevalenceRate).toInt
    val result = (0 until SimConfig.population).toList.map(id => new Person(id))
    for (id <- 0 until numInfected) result(id).infected = true
    result
  }

  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    //
    // to complete with simulation logic
    //
  }
}
