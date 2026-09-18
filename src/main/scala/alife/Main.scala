package alife

import alife.sound.{DefaultSynthesizer, SoundWriterJob}
import alife.util.{DelayGate, SwingEx}
import alife.util.Loops.*

import java.awt.*
import java.awt.event.{ActionEvent, MouseAdapter, MouseEvent}
import java.awt.image.BufferedImage
import java.io.FileReader
import java.util.concurrent.LinkedBlockingDeque
import java.util.concurrent.atomic.AtomicBoolean
import java.util.{Locale, Properties}
import javax.swing.*
import scala.annotation.tailrec

/**
 * A first attempt to run the bacteria system
 */
object Main:
  private def loadProperties(args: Array[String]): Properties =
    val properties = Properties()
    val fileName = if args.length == 0 then "config.properties" else args(0)
    val propReader = FileReader(fileName)
    properties.load(propReader)
    propReader.close()
    properties

  private def brush(fontSize: Int, label: JLabel): JLabel =
    label.setForeground(Color.WHITE)
    label.setFont(label.getFont.deriveFont(fontSize.toFloat))
    label.setAlignmentX(Component.LEFT_ALIGNMENT)
    label

  private def brush(fontSize: Int, button: JToggleButton): JToggleButton =
    button.setFont(button.getFont.deriveFont(fontSize.toFloat))
    button

  private def wellAlignedBox(textWidth: Int, fontSize: Int): Component =
    val dim = Dimension(textWidth, fontSize)
    val rv = Box.Filler(dim, dim, dim)
    rv.setAlignmentX(Component.LEFT_ALIGNMENT)
    rv

  private def makeMonotoneIcon(size: Int, color: Color): ImageIcon =
    val image = BufferedImage(size, size, BufferedImage.TYPE_INT_ARGB)
    val rgb = color.getRGB
    loopFromUntil(0, size): y =>
      loopFromUntil(0, size): x =>
        image.setRGB(x, y, rgb)
    ImageIcon(image)

  private class StatText(fontSize: Int, prefix: String) extends JLabel:
    brush(fontSize, this)
    def setValue(suffix: String): Unit = setText(prefix + suffix)

  @tailrec
  private def drainClickQueue(queue: LinkedBlockingDeque[(Simulation, StepStatistics) => Unit],
                              context: Simulation, stats: StepStatistics): Unit =
    val next = queue.pollFirst()
    if next != null then
      next.apply(context, stats)
      drainClickQueue(queue, context, stats)

  def main(args: Array[String]): Unit =
    System.setProperty("awt.useSystemAAFontSettings", "on")
    System.setProperty("swing.aatext", "true")
    val properties = loadProperties(args)

    val msg = Messages(properties.getProperty("language"))

    val config = Config.parse(properties)
    val compatibleMonster = Monsters.chooseFor(config.actions)
    
    val useSound = properties.getProperty("sound").toBoolean
    val soundFrequency = properties.getProperty("soundFrequency").toFloat
    val soundBufferSize = properties.getProperty("soundBufferSize").toInt
    val textWidth = properties.getProperty("textWidth").toInt
    val fontSize = properties.getProperty("fontSize").toInt

    val pixelScale = properties.getProperty("pixelScale").toInt

    val smallRadius = properties.getProperty("smallRadius").toInt
    val largeRadius = properties.getProperty("largeRadius").toInt
    val enableGenomeDumping = properties.getProperty("enableGenomeDumping").toBoolean
    val enableLegend = properties.getProperty("enableLegend").toBoolean
    val legendIsOnRight = properties.getProperty("legendIsOnRight").toBoolean
    val autoPause = properties.getProperty("autoPause").toInt
    
    val fieldDelayGate = DelayGate()
    val simulationDelayGate = DelayGate()
    val labelDelayGate = DelayGate()
    
    val view = FieldVisualizer(config.fieldWidth, config.fieldHeight, pixelScale, fieldDelayGate)
    view.setBackground(Color.BLACK)

    val rightPane = JPanel()
    rightPane.setLayout(BoxLayout(rightPane, BoxLayout.PAGE_AXIS))
    rightPane.setBackground(Color.BLACK)
    rightPane.setBorder(BorderFactory.createLineBorder(Color.BLACK, 10))

    rightPane.add(brush(fontSize, JLabel(msg.legend)))
    rightPane.add(brush(fontSize, JLabel(msg.legendForBacteria, makeMonotoneIcon(fontSize, Color.RED), SwingConstants.LEADING)))
    rightPane.add(brush(fontSize, JLabel(msg.legendForFood, makeMonotoneIcon(fontSize, Color.GREEN), SwingConstants.LEADING)))
    rightPane.add(brush(fontSize, JLabel(msg.legendForJunk, makeMonotoneIcon(fontSize, Color.BLUE), SwingConstants.LEADING)))
    rightPane.add(brush(fontSize, JLabel(msg.legendForSelection, makeMonotoneIcon(fontSize, Color.MAGENTA), SwingConstants.LEADING)))
    rightPane.add(wellAlignedBox(textWidth, fontSize))

    val statTime = StatText(fontSize, msg.statsTimePassed)
    val statSimFPS = StatText(fontSize, msg.statsSimulationFPS)
    val statVisFPS = StatText(fontSize, msg.statsVisualizationFPS)
    val statNBacteria = StatText(fontSize, msg.statsCountAlive)
    val statNMonsters = StatText(fontSize, msg.statsCountMonsters)
    val statSumEnergy = StatText(fontSize, msg.statsFood)

    rightPane.add(brush(fontSize, JLabel(msg.statsOverall)))
    rightPane.add(statTime)
    rightPane.add(statSimFPS)
    rightPane.add(statVisFPS)
    rightPane.add(statNBacteria)
    rightPane.add(statNMonsters)
    rightPane.add(statSumEnergy)
    rightPane.add(wellAlignedBox(textWidth, fontSize))
    
    val statAverageHealth = StatText(fontSize, msg.statsHealth)
    val statAverageGenome = StatText(fontSize, msg.statsGenomeLength)
    val statAverageInstructions = StatText(fontSize, msg.statsNecessaryInstructions)
    val statAverageInstrRatio = StatText(fontSize, msg.statsNecessaryInstructionRatio)

    rightPane.add(brush(fontSize, JLabel(msg.statsAverage)))
    rightPane.add(statAverageHealth)
    rightPane.add(statAverageGenome)
    rightPane.add(statAverageInstructions)
    rightPane.add(statAverageInstrRatio)
    rightPane.add(wellAlignedBox(textWidth, fontSize))
    
    val statMaxLifeSpan = StatText(fontSize, msg.statsLifeSpan)
    val statMaxHealth = StatText(fontSize, msg.statsHealth)
    val statMaxGenome = StatText(fontSize, msg.statsGenomeLength)
    val statMaxChildren = StatText(fontSize, msg.statsNumChildren)
    val statMaxDistance = StatText(fontSize, msg.statsDistanceTravelled)
    val statMaxSpeed = StatText(fontSize, msg.statsSpeed)

    rightPane.add(brush(fontSize, JLabel(msg.statsMaximum)))
    rightPane.add(statMaxHealth)
    rightPane.add(statMaxLifeSpan)
    rightPane.add(statMaxGenome)
    rightPane.add(statMaxChildren)
    rightPane.add(statMaxDistance)
    rightPane.add(statMaxSpeed)
    rightPane.add(wellAlignedBox(textWidth, fontSize))

    val actionStats = config.actions.map(a => StatText(fontSize, s"${a.toString}: "))

    rightPane.add(brush(fontSize, JLabel(msg.statsActions)))
    for a <- actionStats do rightPane.add(a)
    rightPane.add(wellAlignedBox(textWidth, fontSize))

    val mouseDoNothing = brush(fontSize, JToggleButton(msg.mouseClickNothing))
    val mouseSmallFood = brush(fontSize, JToggleButton(msg.mouseClickFoodSmall))
    val mouseLargeFood = brush(fontSize, JToggleButton(msg.mouseClickFoodLarge))
    val mouseSmallDestroy = brush(fontSize, JToggleButton(msg.mouseClickNukeSmall))
    val mouseLargeDestroy = brush(fontSize, JToggleButton(msg.mouseClickNukeLarge))
    val mouseDumpGenome = brush(fontSize, JToggleButton(msg.mouseClickPrintGenome))
    val mousePutMonster = brush(fontSize, JToggleButton(msg.mouseClickAddMonster))
    
    mousePutMonster.setEnabled(compatibleMonster.nonEmpty)

    val clickCommands = LinkedBlockingDeque[(Simulation, StepStatistics) => Unit]()
    val mouseClickGroup = ButtonGroup()
    mouseClickGroup.add(mouseDoNothing)
    mouseClickGroup.add(mouseSmallFood)
    mouseClickGroup.add(mouseLargeFood)
    mouseClickGroup.add(mouseSmallDestroy)
    mouseClickGroup.add(mouseLargeDestroy)
    mouseClickGroup.add(mouseDumpGenome)
    mouseClickGroup.add(mousePutMonster)
    mouseDoNothing.setSelected(true)

    rightPane.add(brush(fontSize, JLabel(msg.mouseClick)))
    rightPane.add(mouseDoNothing)
    rightPane.add(mouseSmallFood)
    rightPane.add(mouseLargeFood)
    rightPane.add(mouseSmallDestroy)
    rightPane.add(mouseLargeDestroy)
    if enableGenomeDumping then rightPane.add(mouseDumpGenome)
    rightPane.add(mousePutMonster)
    rightPane.add(wellAlignedBox(textWidth, fontSize))

    var lastMagentaLabel = 0

    val magentaNone = brush(fontSize, JToggleButton(new AbstractAction(msg.highlightNothing) {
      override def actionPerformed(e: ActionEvent): Unit = view.setMagentaLabel(-2)
    }))
    val magentaMonster = brush(fontSize, JToggleButton(new AbstractAction(msg.highlightMonsters) {
      override def actionPerformed(e: ActionEvent): Unit = view.setMagentaLabel(-1)
    }))
    val magentaLongestGenome = brush(fontSize, JToggleButton(new AbstractAction(msg.highlightLongest) {
      override def actionPerformed(e: ActionEvent): Unit =
        lastMagentaLabel += 1
        view.setMagentaLabel(lastMagentaLabel)
        clickCommands.addLast((e, _) => e.field.findAndMarkLongestGenome(lastMagentaLabel))
    }))
    val magentaMostProductive = brush(fontSize, JToggleButton(new AbstractAction(msg.highlightMaxChildren) {
      override def actionPerformed(e: ActionEvent): Unit =
        lastMagentaLabel += 1
        view.setMagentaLabel(lastMagentaLabel)
        clickCommands.addLast((e, _) => e.field.findAndMarkMostProductive(lastMagentaLabel))
    }))
    val magentaFastest = brush(fontSize, JToggleButton(new AbstractAction(msg.highlightFastest) {
      override def actionPerformed(e: ActionEvent): Unit =
        lastMagentaLabel += 1
        view.setMagentaLabel(lastMagentaLabel)
        clickCommands.addLast((e, _) => e.field.findAndMarkFastest(lastMagentaLabel))
    }))

    val magentaGroup = ButtonGroup()
    magentaGroup.add(magentaNone)
    magentaGroup.add(magentaMonster)
    magentaGroup.add(magentaLongestGenome)
    magentaGroup.add(magentaMostProductive)
    magentaGroup.add(magentaFastest)
    magentaMonster.setSelected(true)

    rightPane.add(brush(fontSize, JLabel(msg.highlight)))
    rightPane.add(magentaNone)
    rightPane.add(magentaMonster)
    rightPane.add(magentaLongestGenome)
    rightPane.add(magentaMostProductive)
    rightPane.add(magentaFastest)
    rightPane.add(Box.createVerticalGlue())

    val pauseButton = JButton(msg.pause)
    val paused = AtomicBoolean(false)
    
    enum PausedState:
      case Running, Paused, Finished
    
    def executePause(newPaused: PausedState): Unit = newPaused match
      case PausedState.Running =>
        paused.set(false)
        pauseButton.setText(msg.pause)
        pauseButton.setEnabled(true)
      case PausedState.Paused =>
        paused.set(true)
        pauseButton.setText(msg.resume)
        pauseButton.setEnabled(true)
      case PausedState.Finished =>
        paused.set(true)
        pauseButton.setText(msg.finished)
        pauseButton.setEnabled(false)
    
    pauseButton.setBackground(Color.BLUE.darker().darker())
    pauseButton.setForeground(Color.WHITE)
    pauseButton.setFont(pauseButton.getFont.deriveFont(fontSize.toFloat * 2))
    pauseButton.setAlignmentX(Component.LEFT_ALIGNMENT)
    pauseButton.addActionListener: ev =>
      executePause(if paused.get() then PausedState.Running else PausedState.Paused)
    
    rightPane.add(pauseButton)
    
    val restartButton = JButton(msg.restart)
    val restarted = AtomicBoolean(false)
    restartButton.setBackground(Color.RED.darker().darker())
    restartButton.setForeground(Color.WHITE)
    restartButton.setFont(restartButton.getFont.deriveFont(fontSize.toFloat * 2))
    restartButton.setAlignmentX(Component.LEFT_ALIGNMENT)
    restartButton.addActionListener: ev =>
      lastMagentaLabel = 0
      mouseDoNothing.setSelected(true)
      magentaMonster.setSelected(true)
      restarted.set(true)
      executePause(PausedState.Running)

    rightPane.add(restartButton)

    val mouseClickHandler = new MouseAdapter:
      override def mouseClicked(e: MouseEvent): Unit =
        val (x, y) = view.translate(e.getX, e.getY)
        
        // FAT WARNING HERE
        // Almost all these actions change the state of the field (including the RNG)
        // We need to deal with it once we support replays, either by disabling this or by logging this.
        
        if mouseSmallFood.isSelected then clickCommands.addLast((e, s) => e.increaseFood(x, y, smallRadius, s.maxFood * 1.05))
        if mouseLargeFood.isSelected then clickCommands.addLast((e, s) => e.increaseFood(x, y, largeRadius, s.maxFood * 1.05))
        if mouseSmallDestroy.isSelected then clickCommands.addLast((e, _) => e.eraseEverything(x, y, smallRadius))
        if mouseLargeDestroy.isSelected then clickCommands.addLast((e, _) => e.eraseEverything(x, y, largeRadius))
        if mouseDumpGenome.isSelected then clickCommands.addLast: (e, _) =>
          e.findAndDumpClosestIndividual(x, y, 21) match
            case Some(ind) => println(ind.genome.mkString("IArray(", ", ", ")"))
            case None => println("No bacteria nearby")
        if mousePutMonster.isSelected then clickCommands.addLast: (e, _) =>
          val cell = e.field.getCell(x, y)
          val monster = Individual(compatibleMonster.get, -1)
          cell.setIndividual(monster, e.random.nextInt(4), config.initialHealth)
    
    view.addMouseListener(mouseClickHandler)

    val window = JFrame(msg.title)
    window.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    window.setLayout(BorderLayout())
    window.add(view, BorderLayout.CENTER)
    if enableLegend then window.add(rightPane, if legendIsOnRight then BorderLayout.LINE_END else BorderLayout.LINE_START)
    window.setExtendedState(Frame.MAXIMIZED_BOTH)
    window.setUndecorated(true)
    window.setVisible(true)

    val soundWriter = if useSound then
      val job = SoundWriterJob(DefaultSynthesizer, soundFrequency, soundBufferSize)
      val soundThread = Thread(job, "Sound thread")
      soundThread.setDaemon(true)
      soundThread.start()
      Some(job)
    else None

    fieldDelayGate.reset()
    labelDelayGate.reset()
    simulationDelayGate.reset()
    
    fieldDelayGate.setDelay(1e-3) // somewhat of a failsafe: the actual FPS on a commodity display cannot be 1000
    labelDelayGate.setDelay(1e-3) // same
    
    inline def waitUntilUnpausedOrDead(): Unit =
      while paused.get() && window.isVisible do Thread.sleep(100)
      fieldDelayGate.reset()
      labelDelayGate.reset()
      simulationDelayGate.reset()
    
    @tailrec
    def work(sim: Simulation): Unit = if !window.isVisible then
      // terminate various threads
      soundWriter.foreach(_.clearField())
    else if restarted.getAndSet(false) then
      view.resetState()
      // FAT WARNING HERE
      // When replays get supported, check in which way this is compatible
      work(Simulation(config))
    else if !sim.canContinue then
      executePause(PausedState.Finished)
      waitUntilUnpausedOrDead()
      work(null) // logic on what to do after this method quits is written above; `null` just in case
    else
      soundWriter.foreach(_.setField(sim.field))
      
      val stepStats = simulationDelayGate.runOrWait(sim.simulationStep())
      val effectiveFPS = 1.0 / simulationDelayGate.lastLeadInTime
      val visualFPS = 1.0 / fieldDelayGate.lastLeadInTime
      view.fetchField(sim.field)
      
      drainClickQueue(clickCommands, sim, stepStats)
      labelDelayGate.runOrSkip:
        SwingEx.invokeLater:
          statTime.setValue(stepStats.iteration.toString)
          statSimFPS.setValue(String.format(Locale.US, "%.2f", effectiveFPS))
          statVisFPS.setValue(String.format(Locale.US, "%.2f", visualFPS))
          statNBacteria.setValue(stepStats.numberOfBacteria.toString)
          statMaxGenome.setValue(stepStats.maxGenomeSize.toString)
  
          loopFromUntil(0, actionStats.length): i =>
            actionStats(i).setValue(stepStats.actionCounts(i).toString)
  
          statNMonsters.setValue(stepStats.nMonsters.toString)
          statAverageHealth.setValue(String.format(Locale.US, "%.2f", stepStats.averageHealth))
          statAverageGenome.setValue(String.format(Locale.US, "%.2f", stepStats.averageGenomeSize))
          statAverageInstructions.setValue(String.format(Locale.US, "%.2f", stepStats.avgNecessaryInstructions))
          statAverageInstrRatio.setValue(String.format(Locale.US, "%.2f", stepStats.avgNecessaryInstructionRatio))
          statMaxHealth.setValue(String.format(Locale.US, "%.2f", stepStats.maximalHealth))
          statSumEnergy.setValue(String.format(Locale.US, "%.2f", stepStats.totalFood))
          statMaxSpeed.setValue(String.format(Locale.US, "%.2f", stepStats.maxSpeed))
          statMaxChildren.setValue(stepStats.maxChildren.toString)
          statMaxDistance.setValue(stepStats.maxTravelDistance.toString)
          statMaxLifeSpan.setValue(stepStats.maxLife.toString)
      
      if autoPause > 0 && stepStats.iteration % autoPause == 0 then
        SwingEx.invokeAndWait:
          executePause(PausedState.Paused)
      
      if paused.get() then waitUntilUnpausedOrDead()

      work(sim)
    end work
    
    work(Simulation(config))
  end main
