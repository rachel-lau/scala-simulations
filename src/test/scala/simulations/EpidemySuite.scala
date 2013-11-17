package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EpidemySuite extends FunSuite {

  test("prevalence rate"){
    val prevalenceRate = 0.01

    val es = new EpidemySimulator
    val numInfected = es.persons.count(_.infected)

    assert(numInfected == es.SimConfig.population * prevalenceRate,
      "prevalence rate should be 0.01"
      )
  }

  test("all people are allocated to rooms") {
    val es = new EpidemySimulator
    val roomSizes = for (r <- 0 until es.SimConfig.roomRows; 
      c <- 0 until es.SimConfig.roomColumns) yield es.rooms(r)(c).size
    assert(roomSizes.sum == es.SimConfig.population, "all people are allocated to rooms")
  }

  test("find correct neightbours of room(0,0)") {
    val es = new EpidemySimulator
    val room = es.rooms(0)(0)
    val left = room.toLeft // Room(0,7)
    val right = room.toRight // Room(0,1)
    val up = room.toUp // Room(1,0)
    val down = room.toDown // Room(7,0)
    assert(left.row == room.row && left.col == (es.SimConfig.roomColumns - 1))
    assert(right.row == room.row && right.col == 1)
    assert(up.row == 1 && up.col == room.col)
    assert(down.row == (es.SimConfig.roomRows - 1) && up.col == room.col)
  }

  test("find correct neightbours of room(7,7)") {
    val es = new EpidemySimulator
    val room = es.rooms(7)(7)
    val left = room.toLeft // Room(7,6)
    val right = room.toRight // Room(7,0)
    val up = room.toUp // Room(0,7)
    val down = room.toDown // Room(6,7)
    assert(left.row == room.row && left.col == room.col - 1)
    assert(right.row == room.row && right.col == 0)
    assert(up.row == 0 && up.col == room.col)
    assert(down.row == room.row - 1 && up.col == room.col)
  }

  test("find correct neightbours of room(7,0)") {
    val es = new EpidemySimulator
    val room = es.rooms(7)(0)
    val left = room.toLeft // Room(7,7)
    val right = room.toRight // Room(7,1)
    val up = room.toUp // Room(0,0)
    val down = room.toDown // Room(6,0)
    assert(left.row == room.row && left.col == es.SimConfig.roomColumns - 1)
    assert(right.row == room.row && right.col == 1)
    assert(up.row == 0 && up.col == 0)
    assert(down.row == room.row - 1 && down.col == 0)
  }

  test("find correct neightbours of room(0,7)") {
    val es = new EpidemySimulator
    val room = es.rooms(0)(7)
    val left = room.toLeft // Room(0,6)
    val right = room.toRight // Room(0,0)
    val up = room.toUp // Room(1,7)
    val down = room.toDown // Room(7,7)
    assert(left.row == room.row && left.col == room.col - 1)
    assert(right.row == room.row && right.col == 0)
    assert(up.row == 1 && up.col == room.col)
    assert(down.row == es.SimConfig.roomRows - 1 && down.col == room.col)
  }

  test("room is not infected") {
    val es = new EpidemySimulator
    val person = es.persons.head
    val room = es.rooms(person.row)(person.col)
    person.infected = false
    person.sick = false
    person.immune = false
    person.dead = false
    assert(!room.isInfected, "room is not infected")
  }

  test("room is infected by a sick person") {
    val es = new EpidemySimulator
    val person = es.persons.head
    val room = es.rooms(person.row)(person.col)
    person.infected = true
    person.sick = true
    person.immune = false
    person.dead = false
    assert(room.isInfected, "room is infected by a sick person")
  }

  test("room is infected by a dead person") {
    val es = new EpidemySimulator
    val person = es.persons.head
    val room = es.rooms(person.row)(person.col)
    person.infected = true
    person.sick = true
    person.immune = false
    person.dead = true
    assert(room.isInfected, "room is infected by a dead person")
  }

  test("dead person stays dead"){
    val es = new EpidemySimulator

    val chosenOne = es.persons.head
    chosenOne.infected = true
    chosenOne.sick = true
    chosenOne.dead = true
    chosenOne.immune = false

    val(row, col) = (chosenOne.row, chosenOne.col)

    val testDays = 100

    while(!es.agenda.isEmpty && es.agenda.head.time < testDays){
      es.next

      assert(chosenOne.dead == true, "Dead person should keep dead state")
      assert(chosenOne.infected == true, "Dead person keeps infected")
      assert(chosenOne.immune == false, "Dead person cannot become immune")
      assert(chosenOne.sick == true, "Dead person keeps sick")
      assert(chosenOne.col == col && chosenOne.row == row, "Dead person cannot move")
    }
  }

  test("life cycle"){
    var personDied = true;
    while(!personDied){
      val es = new EpidemySimulator

      val incubationTime = 6
      val dieTime = 14
      val immuneTime = 16
      val healTime = 18

      val prevalenceRate = 0.01
      val transRate = 0.4
      val dieRate = 0.25

      val infectedPerson = (es.persons.find{_.infected}).get

      //before incubation time
    	while(es.agenda.head.time < incubationTime){
    		assert(infectedPerson.infected == true, "Infected person keeps infected in 6 days")
    		assert(infectedPerson.sick == false, "Infected person does not get sick in 6 days")
    		assert(infectedPerson.immune == false, "Infected person cannot become immune in 6 days")
    		assert(infectedPerson.dead == false, "Infected person does not die in 6 days")
    		es.next
    	}

      //incubation time has passed, there should be an event for getting sick
      assert(es.agenda.head.time == incubationTime, "You should set a 'sick' event after incubation time")
      while(es.agenda.head.time == incubationTime) es.next
      assert(infectedPerson.sick == true, "Infected person should become sick after 6 days")

      //wait for dieTime
      while(es.agenda.head.time < dieTime){
      	assert(infectedPerson.infected == true, "Sick person keeps infected")
      	assert(infectedPerson.sick == true, "Sick person keeps sick before turning immune")
      	assert(infectedPerson.immune == false, "Sick person is not immune")
      	assert(infectedPerson.dead == false, "Sick person does not die before 14 infected days")
      	es.next
      }

      assert(es.agenda.head.time == dieTime, "You should set a 'die' event (decides with a probability 25% whether the person dies) after 14 days")
      while(es.agenda.head.time == dieTime) es.next
    }
  }


  test("transmissibility rate"){
	  var infectedTimes = 0
	  for(i <- 0 to 100){
		  val es = new EpidemySimulator
		  val healthyPerson = (es.persons find {p => !p.infected}).get
		  es.persons.filter(p => p != healthyPerson) foreach {_.infected = true}

      while(es.agenda.head.time < 6) es.next

      infectedTimes = infectedTimes + (if(healthyPerson.infected) 1 else 0)
	  }
	  assert(infectedTimes > 0, "A person should get infected according to the transmissibility rate when he moves into a room with an infectious person")
  }
}