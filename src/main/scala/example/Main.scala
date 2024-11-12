import breeze.stats.distributions._
import scala.util.Random
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.chart.{LineChart, NumberAxis, XYChart}
import scalafx.scene.control.Label
import scalafx.scene.layout.VBox


sealed trait Road {
  def id: String
  def length: Double
}

case class Highway(id: String, length: Double, lanes: Int) extends Road
case class Street(id: String, length: Double) extends Road

case class Node(id: String, latitude: Double, longitude: Double, edgeOutcomes: Map[String, Double])
case class Edge(id: String, from: Node, to: Node, road: Road, speedLimit: Double)

case class RoadNetwork(nodes: Set[Node], edges: Set[Edge])


sealed trait VehicleType
case object Car extends VehicleType
case object Truck extends VehicleType

case class VehicleInjection(edge: Edge, rate: Double, speed: Double, peakHour: Int, vehicleType: VehicleType)
case class RoadNetworkWithInjection(roadNetwork: RoadNetwork, injections: List[VehicleInjection])

case class VehicleState(id: String, edge: Option[Edge], position: Option[Double], speed: Double)
case class RoadNetworkState(vehicleStates: Set[VehicleState], timestamp: Long)


trait RoadTrafficSimulator {
  def simulate(roadTraffic: RoadNetworkWithInjection, timeStep: Long): Seq[RoadNetworkState]
}


object ImprovedRoadTrafficSimulator extends RoadTrafficSimulator {
  implicit val rand: RandBasis = RandBasis.withSeed(System.currentTimeMillis().toInt)
  val random = new Random()

  def simulate(roadTraffic: RoadNetworkWithInjection, timeStep: Long): Seq[RoadNetworkState] = {
    var currentStates: Set[VehicleState] = Set.empty
    var statesHistory: Seq[RoadNetworkState] = Seq.empty

    
    for (time <- 0L until timeStep) {
      
      roadTraffic.injections.foreach { injection =>
        val poisson = Poisson(injection.rate)(rand)
        val numberOfVehicles = poisson.sample()
        (1 to numberOfVehicles).foreach { i =>
          val vehicleId = s"Vehicle_${time}_$i"
          val initialPosition = 0.0
          val vehicleState = VehicleState(vehicleId, Some(injection.edge), Some(initialPosition), injection.speed)
          currentStates += vehicleState
        }
      }

      
      currentStates = currentStates.flatMap { vehicle =>
        vehicle.edge match {
          case Some(edge) =>
            val newPosition = vehicle.position.getOrElse(0.0) + vehicle.speed * (1.0 / 60.0) 

            if (newPosition >= edge.road.length) {
             
              if (random.nextDouble() < 0.2) { 
                None 
              } else {
                
                val nextEdgeIdOption = weightedRandomChoice(edge.to.edgeOutcomes)
                val nextEdge = roadTraffic.roadNetwork.edges.find(_.id == nextEdgeIdOption)
                nextEdge.map(next => vehicle.copy(edge = Some(next), position = Some(0.0), speed = Math.min(vehicle.speed, next.speedLimit)))
              }
            } else {
              
              Some(vehicle.copy(position = Some(newPosition)))
            }
          case None => None 
        }
      }

      
      statesHistory :+= RoadNetworkState(currentStates, time)
    }


    statesHistory
  }


  private def weightedRandomChoice(outcomes: Map[String, Double]): String = {
    val totalWeight = outcomes.values.sum
    val randValue = random.nextDouble() * totalWeight
    var cumulativeWeight = 0.0

    for ((outcome, weight) <- outcomes) {
      cumulativeWeight += weight
      if (randValue <= cumulativeWeight) {
        return outcome
      }
    }
    outcomes.keys.head 
  }
}



object RoadTrafficSimulationApp extends JFXApp {

  val nodeA = Node("A", 48.8566, 2.3522, Map("edgeAB" -> 0.8, "edgeAC" -> 0.2))
  val nodeB = Node("B", 48.8584, 2.2945, Map("edgeBA" -> 0.5, "edgeBC" -> 0.5))
  val nodeC = Node("C", 48.8606, 2.3376, Map("edgeCA" -> 0.3, "edgeCB" -> 0.7))

  val road1 = Street("road1", 5.0)
  val road2 = Highway("road2", 10.0, 3)
  val edgeAB = Edge("edgeAB", nodeA, nodeB, road1, 50.0)
  val edgeBC = Edge("edgeBC", nodeB, nodeC, road2, 100.0)
  val edgeCA = Edge("edgeCA", nodeC, nodeA, road1, 50.0)

  val roadNetwork = RoadNetwork(Set(nodeA, nodeB, nodeC), Set(edgeAB, edgeBC, edgeCA))


  val injection1 = VehicleInjection(edgeAB, rate = 5.0, speed = 40.0, peakHour = 8, vehicleType = Car)
  val injection2 = VehicleInjection(edgeBC, rate = 3.0, speed = 60.0, peakHour = 18, vehicleType = Truck)
  val roadNetworkWithInjection = RoadNetworkWithInjection(roadNetwork, List(injection1, injection2))

  val simulator = ImprovedRoadTrafficSimulator
  val simulationResult = simulator.simulate(roadNetworkWithInjection, timeStep = 60)

  stage = new JFXApp.PrimaryStage {
    title.value = "Road Traffic Simulation"
    scene = new Scene {
      val xAxis = NumberAxis("Time Step")
      val yAxis = NumberAxis("Number of Vehicles")
      val lineChart = new LineChart[Number, Number](xAxis, yAxis)
      lineChart.title = "Number of Vehicles Over Time"

      val series = new XYChart.Series[Number, Number]
      series.name = "Vehicles in Network"

      simulationResult.zipWithIndex.foreach { case (state, index) =>
        series.getData.add(XYChart.Data[Number, Number](index, state.vehicleStates.size))
      }

      lineChart.getData.add(series)

      content = new VBox {
        children = Seq(
          new Label("Road Traffic Simulation Results"),
          lineChart
        )
      }
    }
  }
}


object RoadTrafficSimulationConsoleTest extends App {
  val nodeA = Node("A", 48.8566, 2.3522, Map("edgeAB" -> 0.8, "edgeAC" -> 0.2))
  val nodeB = Node("B", 48.8584, 2.2945, Map("edgeBA" -> 0.5, "edgeBC" -> 0.5))
  val nodeC = Node("C", 48.8606, 2.3376, Map("edgeCA" -> 0.3, "edgeCB" -> 0.7))

  val road1 = Street("road1", 5.0)
  val road2 = Highway("road2", 10.0, 3)
  val edgeAB = Edge("edgeAB", nodeA, nodeB, road1, 50.0)
  val edgeBC = Edge("edgeBC", nodeB, nodeC, road2, 100.0)
  val edgeCA = Edge("edgeCA", nodeC, nodeA, road1, 50.0)

  val roadNetwork = RoadNetwork(Set(nodeA, nodeB, nodeC), Set(edgeAB, edgeBC, edgeCA))

  val injection1 = VehicleInjection(edgeAB, rate = 2.0, speed = 40.0, peakHour = 8, vehicleType = Car)
  val injection2 = VehicleInjection(edgeBC, rate = 1.0, speed = 60.0, peakHour = 18, vehicleType = Truck)
  val roadNetworkWithInjection = RoadNetworkWithInjection(roadNetwork, List(injection1, injection2))

  val simulator = ImprovedRoadTrafficSimulator
  val timeSteps = 10
  val simulationResult = simulator.simulate(roadNetworkWithInjection, timeSteps)

  println(s"Simulation results for $timeSteps time steps:")
  simulationResult.zipWithIndex.foreach { case (state, index) =>
    println(s"Time Step $index:")
    println(s"  Vehicles in Network: ${state.vehicleStates.size}")
    state.vehicleStates.foreach { vehicleState =>
      println(s"    - Vehicle ID: ${vehicleState.id}, Position: ${vehicleState.position.getOrElse(0.0)}, Speed: ${vehicleState.speed}")
    }
  }
}