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
    for (id <- 0 until numInfected) people(id).becomeInfected
    for (person <- people) 
      rooms(person.row)(person.col) allocate person
    people
  }

  // The transmissiblityRate is 0.40 means 2 out of 5
  def randomInfected() : Boolean = {
    val rand = randomBelow(5)
    rand == 0 || rand == 1
  }

  // The dead rate is 0.25 means 1 out of 4
  def randomDead() : Boolean = {
    val rand = randomBelow(4)
    rand == 0
  }

  def printRooms() {
    for (row <- 0 until roomRows; col <- 0 until roomColumns)
      println("(" + row + "," + col + ")=" + rooms(row)(col))
  }

  class Room(val row: Int, val col: Int) {
    var people: Set[Person] = new collection.immutable.HashSet()

    def allocate(person: Person) {
      people = people + person
      person.row = row
      person.col = col
    }

    def enter(person: Person) {
      allocate(person)

      // A person may get infected if the room has infectious people.
      // However, an immune person cannot get infected
      if (!person.isInfected) {
        if (isInfectious && !person.isImmune) {
          if (randomInfected) {
            person.becomeInfected
          }
        }
      }
    }

    def leave(person: Person) {
      people = people - person
    }

    def isInfectious() : Boolean = {
      val infectious = people filter (p => p.isInfectious)
      !infectious.isEmpty
    }

    def isVisiblyInfectious() : Boolean = {
      val infectious = people filter (p => p.isVisiblyInfectious)
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

    def isInfected() : Boolean = {
      infected
    }

    def isImmune() : Boolean = {
      immune
    }

    def isInfectious() : Boolean = {
      infected || sick || immune || dead
    }

    def isVisiblyInfectious() : Boolean = {
      sick || dead
    }

    def becomeInfected() {
      if (!dead) {
        infected = true
        scheduleToSick()
        scheduleToDie()
        scheduleToImmune()
        scheduleToHealthy()
      }
    }

    def becomeSick() {
      if (!dead) {
        sick = true 
      }
    }

    def becomeDead() {
      if (!dead) {
        if (randomDead) {
          dead = true 
        }
      }
    }

    def becomeImmune() {
      if (!dead) {
        sick = false
        immune = true 
      }
    }

    def becomeHealthy() {
      if (!dead) {
        infected = false
        sick = false
        immune = false
        dead = false
      }
    }

    // A person avoids rooms which are visibly infectious
    def move() {
      if (!dead) {
        val currentRoom = rooms(row)(col)
        val neighbours = currentRoom.neighbours filter (r => !r.isVisiblyInfectious)
        if (!neighbours.isEmpty) {
          val safeRooms = neighbours.toList
          val nextRoom = randomRoom(safeRooms)
          currentRoom.leave(this)
          nextRoom.enter(this)
        }
        scheduleNextMove
      }
    }

    def randomRoom(rooms: List[Room]) : Room = {
      val len = rooms.length
      val index = randomBelow(len)
      rooms(index)
    }

    // A person moves within the next 5 days
    def scheduleNextMove() {
      val delay = randomBelow(nextMoveDelay) + 1
      afterDelay(delay) { 
        move 
      }
    }

    def scheduleToSick() {
      afterDelay(infectedToSickDelay) {
        becomeSick
      } 
    }

    def scheduleToDie() {
      afterDelay(infectedToDeathDelay) { 
        becomeDead
      } 
    }

    def scheduleToImmune() {
      afterDelay(infectedToImmuneDelay) { 
        becomeImmune
      } 
    }

    def scheduleToHealthy() {
      afterDelay(infectedToHealthyDelay) { 
        becomeHealthy
      } 
    }

    scheduleNextMove
  }
}
