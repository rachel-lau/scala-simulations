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
      world(row)(col) = new Room(row, col)
    world
  }

  val persons: List[Person] = {
    val numInfected = (population * prevalenceRate).toInt
    val people = (0 until population).toList.map(id => new Person(id))
    for (id <- 0 until numInfected) people(id).infected = true
    for (person <- people) 
      rooms(person.row)(person.col) enter person
    people
  }

  def printRooms() {
    for (row <- 0 until roomRows; col <- 0 until roomColumns)
      println("(" + row + "," + col + ")=" + rooms(row)(col))
  }

  class Room(val row: Int, val col: Int) {
    var people: Set[Person] = new collection.immutable.HashSet()

    def enter(person: Person) {
      people = people + person
    }

    def leave(person: Person) {
      people = people - person
    }

    def isInfected() : Boolean = {
      val infectious = people filter (p => p.sick || p.dead)
      !infectious.isEmpty
    }

    def size() : Int = {
      people.size
    }

    def toLeft() : Room = {
      val left = if (col - 1 >= 0) (col - 1) else (roomColumns - 1)
      rooms(row)(left)
    }

    def toRight() : Room = {
      val right = if (col + 1 >= roomColumns) 0 else (col + 1)
      rooms(row)(right)
    }

    def toUp() : Room = {
      val up = if (row + 1 >= roomRows) 0 else (row + 1)
      rooms(up)(col)
    }

    def toDown() : Room = {
      val down = if (row - 1 >= 0) (row - 1) else (roomRows - 1)
      rooms(down)(col)
    }

    def neighbours() : Set[Room] = {
      var neighbours: Set[Room] = new collection.immutable.HashSet()
      neighbours + toLeft + toRight + toUp + toDown
    }

    override def toString() = {
      "Room(" + row + "," + col + ")"
    }
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
