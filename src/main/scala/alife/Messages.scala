package alife

trait Messages:
  def title: String
  def pause: String
  def resume: String
  def restart: String

  def legend: String
  def legendForBacteria: String
  def legendForFood: String
  def legendForJunk: String
  def legendForSelection: String

  def statsOverall: String
  def statsAverage: String
  def statsMaximum: String
  
  def statsCountAlive: String
  def statsTimePassed: String
  def statsCountMonsters: String
  def statsFood: String
  
  def statsActions: String
  def statsHealth: String
  def statsGenomeLength: String
  def statsNecessaryInstructions: String
  def statsNecessaryInstructionRatio: String
  def statsLifeSpan: String
  def statsNumChildren: String
  def statsDistanceTravelled: String
  def statsSpeed: String
  def statsVisualizationFPS: String
  def statsSimulationFPS: String

  def mouseClick: String
  def mouseClickNothing: String
  def mouseClickFoodSmall: String
  def mouseClickFoodLarge: String
  def mouseClickNukeSmall: String
  def mouseClickNukeLarge: String
  def mouseClickPrintGenome: String
  def mouseClickAddMonster: String

  def highlight: String
  def highlightNothing: String
  def highlightMonsters: String
  def highlightLongest: String
  def highlightMaxChildren: String
  def highlightFastest: String

object Messages:
  def apply(lang: String): Messages = English

  private object English extends Messages:
    override def title: String = "Artificial Bacteria"
    override def pause: String = "PAUSE"
    override def resume: String = "RESUME"
    override def restart: String = "BEGIN ANEW"

    override def legend: String = "Legend:"
    override def legendForBacteria: String = "Bacteria"
    override def legendForFood: String = "Food"
    override def legendForJunk: String = "Junk"
    override def legendForSelection: String = "Highlighted ones"
    
    override def statsOverall: String = "General stats:"
    override def statsAverage: String = "Averages:"
    
    override def statsMaximum: String = "Maxima:"
    
    override def statsActions: String = "Total number of actions:"
    override def statsCountAlive: String = "Alive bacteria: "
    override def statsTimePassed: String = "Time passed: "
    override def statsCountMonsters: String = "Of which monsters: "
    override def statsHealth: String = "Health: "
    override def statsGenomeLength: String = "Genome length: "
    override def statsFood: String = "Amount of food: "
    override def statsSimulationFPS: String = "Steps per second: "
    override def statsVisualizationFPS: String = "UI updates per second: "
    override def statsNecessaryInstructions: String = "Necessary instructions: "
    override def statsNecessaryInstructionRatio: String = "Necessary instr ratio: "
    
    override def statsDistanceTravelled: String = "Distance travelled: "
    override def statsLifeSpan: String = "Life span: "
    override def statsNumChildren: String = "No of children: "
    override def statsSpeed: String = "Moves per action: "

    override def mouseClick: String = "Mouse click action:"
    override def mouseClickNothing: String = "Do nothing"
    override def mouseClickFoodSmall: String = "Add food (small radius)"
    override def mouseClickFoodLarge: String = "Add food (large radius)"
    override def mouseClickNukeSmall: String = "Erase all (small radius)"
    override def mouseClickNukeLarge: String = "Erase all (large radius)"
    override def mouseClickPrintGenome: String = "Print genome to console"
    override def mouseClickAddMonster: String = "Add a monster"
    
    override def highlight: String = "Who to highlight:"
    override def highlightNothing: String = "Nobody"
    override def highlightMonsters: String = "Monsters and their children"
    override def highlightLongest: String = "The longest genome"
    override def highlightMaxChildren: String = "Max number of children"
    override def highlightFastest: String = "The fastest bacterium"
