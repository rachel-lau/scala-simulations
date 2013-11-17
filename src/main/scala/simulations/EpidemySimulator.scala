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
    val dealthRate = 0.25
    val transmissibilityRate = 0.40
    val nextMoveDelay: Int = 5
    val infectedToSickDelay: Int = 6
    val infectedToDeathDelay: Int = 14
    val infectedToImmuneDelay: Int = 16
    val infectedToHealthyDelay: Int = 18
  }

  import SimConfig._

  val rooms = {
    val world = Array.ofDim[Room](roomRows, roomColumns)
    for (row <- 0 until roomRows; col <- 0 until roomColumns)
      world(row)(col) = new Room(0, 0, 0, 0, 0)
    world
  }

  val persons: List[Person] = {
    val numInfected = (population * prevalenceRate).toInt
    val people = (0 until population).toList.map(id => new Person(id))
    for (id <- 0 until numInfected) people(id).infected = true
    for (person <- people) 
      rooms(person.row)(person.col) count person
    printRooms
    people
  }

  def printRooms() {
    for (row <- 0 until roomRows; col <- 0 until roomColumns)
      println("(" + row + "," + col + ")=" + rooms(row)(col))
  }

  class Room(var healthy: Int, var infected: Int, var sick: Int, var immune: Int,
    var dead: Int) {
    def reset { healthy = 0; infected = 0; sick = 0; immune = 0; dead = 0 }
    def count(p: Person) {
      if (p.infected) infected += 1
      else if (p.sick) sick += 1
      else if (p.immune) immune += 1
      else if (p.dead) dead += 1
      else healthy += 1
    }
    override def toString() = "Room(" + healthy + ", " + infected + ", " + 
      sick + ", " + immune + ", " + dead + ")"
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
    def move() {
    }

    // A person moves within the next 5 days
    def scheduleNextMove() {
      val delay = randomBelow(nextMoveDelay) + 1
      afterDelay(delay) { move() }
    }

    def scheduleToSick() {
      afterDelay(infectedToSickDelay) {
        sick = true 
      } 
    }

    def scheduleToDie() {
      afterDelay(infectedToDeathDelay) { 
        dead = true 
      } 
    }

    def scheduleToImmune() {
      afterDelay(infectedToImmuneDelay) { 
        immune = true 
      } 
    }

    def scheduleToHealthy() {
      afterDelay(infectedToHealthyDelay) { 
        infected = false
        sick = false
        immune = false
        dead = false
      } 
    }

    // Dead people do not move
    if (!dead) 
      scheduleNextMove
  }
}
