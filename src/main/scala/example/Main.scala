import breeze.stats.distributions.Rand
import scala.util.Random
import breeze.stats.distributions.{Poisson, RandBasis}

//juste un demo il y a encore des erreur et des améliorations 
object TrafficSimulationApp{
  implicit val randBasis: RandBasis = RandBasis.withSeed(Random.nextInt())
  sealed trait Road {
    def id: String
    def length: Double
  }
  case class Highway(id: String, length: Double, lanes: Int) extends Road
  case class Street(id: String, length: Double, isOneWay: Boolean) extends Road

  case class Node(id: String, latitude: Double, longitude: Double, edgeOutcomes: Map[String, Double])
  case class Edge(id: String, from: Node, to: Node, road: Road, speedLimit: Double)

  sealed trait VehicleType
  case object Car extends VehicleType
  case object Truck extends VehicleType

  case class VehicleInjection(edge: Edge, rate: Double, speed: Double, peakHour: Int, vehicleType: VehicleType)
  case class RoadNetwork(nodes: Set[Node], edges: Set[Edge])
  case class RoadNetworkWithInjection(roadNetwork: RoadNetwork, injections: List[VehicleInjection])

  case class VehicleState(id: String, edge: Option[Edge], position: Option[Double], speed: Double)
  case class RoadNetworkState(vehicleStates: Set[VehicleState], timestamp: Long)

  trait RoadTrafficSimulator {
    def simulate(roadTraffic: RoadNetworkWithInjection, timeStep: Long): Seq[RoadNetworkState]
  }
  class SimpleRoadTrafficSimulator extends RoadTrafficSimulator {
  def simulate(roadTraffic: RoadNetworkWithInjection, timeStep: Long): Seq[RoadNetworkState] = {
    val vehicleStates = roadTraffic.injections.flatMap { injection =>
      val poisson = new Poisson(injection.rate)
      val numVehicles = poisson.sample() 
      println(s"Injected $numVehicles vehicles at hour $timeStep for edge ${injection.edge.id}")

      (1 to numVehicles).map { _ =>
        val vehicleId = s"${injection.edge.id}-${Random.nextInt(10000)}"
        VehicleState(vehicleId, Some(injection.edge), Some(0.0), injection.speed)
      }
    }

    val roadNetworkState = RoadNetworkState(vehicleStates.toSet, timeStep)

    Seq(roadNetworkState)
  }
}


  def main(args: Array[String]): Unit = {
    val nodeA = Node("A", 48.8566, 2.3522, Map("B" -> 0.7))
    val nodeB = Node("B", 48.8570, 2.3530, Map("A" -> 0.3))
    val highway = Highway("H1", 5.0, 3)
    val edgeAB = Edge("E1", nodeA, nodeB, highway, 120)

    val network = RoadNetwork(Set(nodeA, nodeB), Set(edgeAB))
    val injections = List(VehicleInjection(edgeAB, rate = 5, speed = 100, peakHour = 8, vehicleType = Car))
    val roadNetworkWithInjection = RoadNetworkWithInjection(network, injections)
    val simulator = new SimpleRoadTrafficSimulator()
    val states = simulator.simulate(roadNetworkWithInjection, timeStep = 3600L) 

    states.foreach(println)
  }
}
