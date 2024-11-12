import breeze.stats.distributions._
import scala.util.Random
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.chart.{LineChart, NumberAxis, XYChart}
import scalafx.scene.control.Label
import scalafx.scene.layout.VBox




/**
 * Trait scellé représentant une route générique dans un réseau routier.
 * 
 * @define id L'identifiant unique de la route.
 * @define length La longueur de la route en kilomètres.
 */
sealed trait Road{
  def id: String
  def length: Double
}

/**
 * Objet représentant une autoroute dans un réseau routier.
 *
 * @param id L'identifiant unique de l'autoroute.
 * @param length La longueur de l'autoroute en kilomètres.
 * @param lanes Le nombre de voies sur l'autoroute.
 */
case class Highway(id:String,length:Double,lanes:Int) extends Road

/**
 * Objet représentant une rue dans un réseau routier.
 *
 * @param id L'identifiant unique de la rue.
 * @param length La longueur de la rue en kilomètres.
 * @param isOneWay Indique si la rue est à sens unique.
 */
case class Street(id:String,length:Double) extends Road

/**
 * Objet représentant un nœud (point ou intersection) dans un réseau routier.
 *
 * @param id L'identifiant unique du nœud.
 * @param latitude La latitude du nœud.
 * @param longitude La longitude du nœud.
 * @param edgeOutcomes Un ensemble d'arêtes possibles à prendre, chacune avec sa probabilité respective.
 */
case class Node(id:String,latitude:Double,longitude:Double,edgeOutcomes:Map[String,Double])


/**
 * Objet représentant une arête dans le réseau routier, c'est-à-dire un segment de route entre deux nœuds.
 *
 * @param from Le nœud de départ de l'arête.
 * @param to Le nœud d'arrivée de l'arête.
 * @param road La route associée à cette arête.
 * @param speedLimit La limite de vitesse sur cette arête en km/h.
 */
case class Edge(id:String,from:Node,to:Node,road:Road,speedLimit:Double)

/**
 * Objet représentant un réseau routier.
 * 
 * @param nodes L'ensemble des nœuds dans le réseau.
 * @param edges L'ensemble des arêtes entre les nœuds.
 * @param stoplights L'ensemble des feux de signalisation dans le réseau.
 */
case class RoadNetwork(nodes:Set[Node],edges:Set[Edge])


/**
 * Trait représentant un type de véhicule générique.
 */
sealed trait VehicleType
case object Car extends VehicleType
case object Truck extends VehicleType

/**
 * Objet décrivant comment les véhicules sont injectés dans le réseau routier à un nœud spécifique.
 *
 * @param edge Le nœud routier et la direction où les véhicules seront injectés.
 * @param rate Le taux moyen d'injection des véhicules par heure.
 * @param peakHour L'heure spécifique où le taux d'injection atteindra son maximum.
 * @param vehicleType Le type de véhicule à injecter.
 */
case class VehicleInjection(edge:Edge,rate:Double,speed:Double,peakHour:Int,vehicleType:VehicleType)

/**
 * Object représentant un réseau routier avec injection de véhicules.
 *
 * @param roadNetwork le réseau routier
 * @param injectionConfig La configuration d'injection de véhicules dans le réseau.
 */
case class RoadNetworkWithInjection(
  roadNetwork:RoadNetwork,
  injections:List[VehicleInjection])


/**
 * Objet représentant l'état d'un véhicule sur une route spécifique du réseau routier.
 *
 * @param id L'identifiant unique du véhicule.
 * @param edge L'artère routière sur lequel le véhicule se trouve.
 * @param position La position du véhicule sur la route en kilomètres depuis le début de la route.
 * @param speed La vitesse actuelle du véhicule en km/h.
 */
case class VehicleState(id:String,edge:Option[Edge],position:Double,speed:Double)

/**
 * Objet représentant l'état complet du réseau routier pour une heure donnée.
 *
 * @param vehicleStates L'état actuel de tous les véhicules sur les routes du réseau.
 * @param timestamp L'instant de cet état.
 */
case class RoadNetworkState(vehicleStates:Set[VehicleState],timestamp:Long)

trait RoadTrafficSimulator{
  def simulate(roadTraffic:RoadNetworkWithInjection,timeStep: Long):Seq[RoadNetworkState]
}


//objet implementant le simulateur de trafic routier
object ImprovedRoadTrafficSimulator extends RoadTrafficSimulator{
  //base aléatoire pour la simulation
  implicit val rand:RandBasis=RandBasis.withSeed(System.currentTimeMillis().toInt)
  val random =new Random()//// Générer un nombre aléatoire
  //compteur pour générer des identifiants uniques pour chaque vehicule
  var vehicleIdCounter: Long=0
//méthode pour simuler le réseau routier pendant un certain nombre de pas de temps
  def simulate(roadTraffic:RoadNetworkWithInjection,timeStep:Long):Seq[RoadNetworkState]={

    var currentStates:Set[VehicleState]=Set.empty//états actuels des vehicules
    var statesHistory:Seq[RoadNetworkState]=Seq.empty//historique des états du réseau

    for(time<-0L until timeStep){
      roadTraffic.injections.foreach{injection=>
        val poisson=Poisson(injection.rate)(rand)
        val numberOfVehicles=poisson.sample()
        (1 to numberOfVehicles).foreach {_=>
          vehicleIdCounter+=1
          val vehicleId=s"Vehicle_$vehicleIdCounter"
          //position initiale du vehicule sur la route
          val initialPosition=0.0
          //crée l'état du véhicule
          val vehicleState=VehicleState(vehicleId,Some(injection.edge),initialPosition,injection.speed)
          //ajoute l'état du véhicule a l'ensemble des états actuels
          currentStates+=vehicleState
        }
      }
      //mise à jour de l'etat des véhicules
      currentStates=currentStates.flatMap{vehicle=>

        vehicle.edge match{
          case Some(edge)=>
            //calcule la nouvelle position du véhicule
            val newPosition=vehicle.position+vehicle.speed*(1.0/60.0)
            //si le véhicule atteint la fin de la route
            if (newPosition>=edge.road.length){
              //véhicule quitte le réseau avec une probabilité de 20%
              if (random.nextDouble()<0.2){
                None
              } else {
                 //choisir la prochaine route en fonction des probabilités
                val nextEdgeIdOption = weightedRandomChoice(edge.to.edgeOutcomes)
                //trouver la prochaine arrete
                val nextEdge = roadTraffic.roadNetwork.edges.find(_.id==nextEdgeIdOption)
                //met à jour l'état du vehicule
                nextEdge.map(next=>vehicle.copy(edge=Some(next),position=0.0,speed=Math.min(vehicle.speed,next.speedLimit)))
              }
            } else {
              Some(vehicle.copy(position=newPosition))//met a jour la position du véhicule
            }
          case None=>None//aucun changement si le véhicule n'est sur aucune route
        }
      }

      //ajoute l'état du réseau a l'historique
      statesHistory:+=RoadNetworkState(currentStates,time)
    }

    statesHistory
  }


  //méthode pour choisir aléatoirement une option basée sur des probabilités pondérees
  private def weightedRandomChoice(outcomes:Map[String,Double]):String={
    val totalWeight=outcomes.values.sum//somme des poids
    val randValue=random.nextDouble()*totalWeight
    //Poids cumulé
    var cumulativeWeight=0.0

    for((outcome, weight)<- outcomes){
      cumulativeWeight+=weight
      if(randValue<=cumulativeWeight){
        //retourne l'option sélectionne
        return outcome
      }
    }
    //retourne la première clé si aucune autre option n'est choisie
    outcomes.keys.head
  }
}



object RoadTrafficSimulationConsoleTest extends App{

  val nodeA=Node("A",48.8566,2.3522,Map("edgeAB"->0.8,"edgeAC"->0.2))
  val nodeB=Node("B",48.8584,2.2945,Map("edgeBA"->0.5,"edgeBC"->0.5))
  val nodeC=Node("C",48.8606,2.3376,Map("edgeCA"->0.3,"edgeCB"->0.7))

  val road1=Street("road1",5.0)
  val road2=Highway("road2",10.0,3)

  val edgeAB=Edge("edgeAB",nodeA,nodeB,road1,50.0)
  val edgeBC=Edge("edgeBC",nodeB,nodeC,road2,100.0)
  val edgeCA=Edge("edgeCA",nodeC,nodeA,road1,50.0)

  //définition du réseau routier
  val roadNetwork=RoadNetwork(Set(nodeA,nodeB,nodeC),Set(edgeAB,edgeBC,edgeCA))

  //injection des véhicules
  val injection1=VehicleInjection(edgeAB,rate=2.0,speed=40.0,peakHour=8,vehicleType=Car)
  val injection2=VehicleInjection(edgeBC,rate=1.0,speed=60.0,peakHour=18,vehicleType=Truck)

  val roadNetworkWithInjection= RoadNetworkWithInjection(roadNetwork,List(injection1,injection2))

  val simulator=ImprovedRoadTrafficSimulator
  //nombre de pas de temps pour la simulation
  val timeSteps=10
  //Lancement de la simulation
  val simulationResult=simulator.simulate(roadNetworkWithInjection,timeSteps)

  //affichage des résultats de la simulation
  println(s"Simulation results for $timeSteps time steps:")
  simulationResult.zipWithIndex.foreach{case(state, index)=>
    println(s"Time Step $index:")
    println(s"  Vehicles in Network: ${state.vehicleStates.size}")
    
    state.vehicleStates.foreach{vehicleState=>
      println(s"- Vehicle ID: ${vehicleState.id}, Position: ${vehicleState.position}, Speed: ${vehicleState.speed}")
    }
  }
}




object RoadTrafficSimulationApp extends JFXApp {
  val nodeA=Node("A",48.8566,2.3522,Map("edgeAB"->0.8,"edgeAC"->0.2))
  val nodeB=Node("B",48.8584,2.2945,Map("edgeBA"->0.5,"edgeBC"->0.5))
  val nodeC=Node("C",48.8606,2.3376,Map("edgeCA"->0.3,"edgeCB"->0.7))

  val road1=Street("road1",5.0)
  val road2=Highway("road2",10.0,3)

  val edgeAB=Edge("edgeAB",nodeA,nodeB,road1,50.0)
  val edgeBC=Edge("edgeBC",nodeB,nodeC,road2,100.0)
  val edgeCA=Edge("edgeCA",nodeC,nodeA,road1,50.0)

  val roadNetwork=RoadNetwork(Set(nodeA,nodeB,nodeC),Set(edgeAB,edgeBC,edgeCA))

  val injection1=VehicleInjection(edgeAB,rate=2.0,speed=40.0,peakHour=8,vehicleType=Car)
  val injection2=VehicleInjection(edgeBC,rate=1.0,speed=60.0,peakHour=18,vehicleType=Truck)

  val roadNetworkWithInjection=RoadNetworkWithInjection(roadNetwork,List(injection1,injection2))

  val simulator=ImprovedRoadTrafficSimulator
  val timeSteps=10
  val simulationResult=simulator.simulate(roadNetworkWithInjection,timeSteps)

  stage=new JFXApp.PrimaryStage{
    title.value = "Road Traffic Simulation"

    scene=new Scene{
      val xAxis=NumberAxis("Time Step")
      val yAxis=NumberAxis("Number of Vehicles")

      val lineChart=new LineChart[Number, Number](xAxis,yAxis)
      lineChart.title ="Number of Vehicles Over Time"

      val series=new XYChart.Series[Number,Number]
      series.name="Vehicles in Network"

      simulationResult.zipWithIndex.foreach{case(state,index)=>
        series.getData.add(XYChart.Data[Number,Number](index,state.vehicleStates.size))
      }

      lineChart.getData.add(series)

      content=new VBox{
        children=Seq(
          new Label("Road Traffic Simulation Results"),
          lineChart
        )
      }
    }
  }
}