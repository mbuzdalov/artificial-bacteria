package alife

import alife.sound.{DefaultSynthesizer, SoundWriterJob}
import alife.util.{Loops, SwingEx}

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
    Loops.foreach(0, size): y =>
      Loops.foreach(0, size): x =>
        image.setRGB(x, y, rgb)
    ImageIcon(image)

  private class StatText(fontSize: Int, prefix: String) extends JLabel:
    brush(fontSize, this)
    def setValue(suffix: String): Unit = setText(prefix + suffix)

  @tailrec
  private def findAndDumpIndividual(field: Field, x: Int, y: Int, d: Int): Unit =
    if d == 11 then println("No bacterium nearby") else
      (-d to d).view.flatMap(dx => Seq(
        Option(field.getCellChecked(x + dx, y + d - math.abs(dx)).individual),
        Option(field.getCellChecked(x + dx, y - d + math.abs(dx)).individual),
      ).flatten).headOption match
        case Some(g) => println(g.genome.mkString("IArray(", ", ", ")"))
        case None => findAndDumpIndividual(field, x, y, d + 1)

  @tailrec
  private def drainClickQueue(queue: LinkedBlockingDeque[(Field, Field.StepStatistics) => Unit],
                              field: Field, stats: Field.StepStatistics): Unit =
    val next = queue.pollFirst()
    if next != null then
      next.apply(field, stats)
      drainClickQueue(queue, field, stats)

  def main(args: Array[String]): Unit =
    System.setProperty("awt.useSystemAAFontSettings", "on")
    System.setProperty("swing.aatext", "true")
    val properties = loadProperties(args)

    val msg = Messages(properties.getProperty("language"))

    val config = Config.parse(properties)
    println(s"Info: using random factory ${config.randomFactory} and random seed ${config.randomSeed}")
    val compatibleMonster = Monsters.chooseFor(config.actions)
    
    val useSound = properties.getProperty("sound").toBoolean
    val soundFrequency = properties.getProperty("soundFrequency").toFloat
    val textWidth = properties.getProperty("textWidth").toInt
    val fontSize = properties.getProperty("fontSize").toInt

    val width = properties.getProperty("fieldWidth").toInt
    val height = properties.getProperty("fieldHeight").toInt
    val pixelScale = properties.getProperty("pixelScale").toInt
    val initialHealth = properties.getProperty("initialHealth").toDouble
    val initialGenomeLength = properties.getProperty("initialGenomeLength").toInt
    val initialBacteriaProbability = properties.getProperty("initialBacteriaProbability").toDouble
    val field = Field(width, height)

    val smallRadius = properties.getProperty("smallRadius").toInt
    val largeRadius = properties.getProperty("largeRadius").toInt
    val enableGenomeDumping = properties.getProperty("enableGenomeDumping").toBoolean
    val enableLegend = properties.getProperty("enableLegend").toBoolean
    val legendIsOnRight = properties.getProperty("legendIsOnRight").toBoolean
    val autoPause = properties.getProperty("autoPause").toInt

    field.initialize(config)
    val view = FieldVisualizer(field, pixelScale)
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
    val statNBacteria = StatText(fontSize, msg.statsCountAlive)
    val statNMonsters = StatText(fontSize, msg.statsCountMonsters)
    val statAverageHealth = StatText(fontSize, msg.statsAvgHealth)
    val statAverageGenome = StatText(fontSize, msg.statsAvgGenome)
    val statSumEnergy = StatText(fontSize, msg.statsFood)

    rightPane.add(brush(fontSize, JLabel(msg.stats)))
    rightPane.add(statTime)
    rightPane.add(statNBacteria)
    rightPane.add(statNMonsters)
    rightPane.add(statAverageHealth)
    rightPane.add(statAverageGenome)
    rightPane.add(statSumEnergy)
    rightPane.add(wellAlignedBox(textWidth, fontSize))

    val statMaxLifeSpan = StatText(fontSize, msg.bestLifeSpan)
    val statMaxHealth = StatText(fontSize, msg.bestHealth)
    val statMaxGenome = StatText(fontSize, msg.maxGenomeSize)
    val statMaxChildren = StatText(fontSize, msg.bestChildren)
    val statMaxDistance = StatText(fontSize, msg.bestDistance)
    val statMaxSpeed = StatText(fontSize, msg.bestSpeed)

    rightPane.add(brush(fontSize, JLabel(msg.best)))
    rightPane.add(statMaxHealth)
    rightPane.add(statMaxLifeSpan)
    rightPane.add(statMaxGenome)
    rightPane.add(statMaxChildren)
    rightPane.add(statMaxDistance)
    rightPane.add(statMaxSpeed)
    rightPane.add(wellAlignedBox(textWidth, fontSize))

    val actionsEat = StatText(fontSize, msg.nActionsFeed)
    val actionsMove = StatText(fontSize, msg.nActionsMove)
    val actionsFork = StatText(fontSize, msg.nActionsFork)
    val actionsCW = StatText(fontSize, msg.nActionsCW)
    val actionsCCW = StatText(fontSize, msg.nActionsCCW)

    rightPane.add(brush(fontSize, JLabel(msg.nActions)))
    rightPane.add(actionsEat)
    rightPane.add(actionsMove)
    rightPane.add(actionsFork)
    rightPane.add(actionsCW)
    rightPane.add(actionsCCW)
    rightPane.add(wellAlignedBox(textWidth, fontSize))

    val mouseDoNothing = brush(fontSize, JToggleButton(msg.mouseClickNothing))
    val mouseSmallFood = brush(fontSize, JToggleButton(msg.mouseClickFoodSmall))
    val mouseLargeFood = brush(fontSize, JToggleButton(msg.mouseClickFoodLarge))
    val mouseSmallDestroy = brush(fontSize, JToggleButton(msg.mouseClickNukeSmall))
    val mouseLargeDestroy = brush(fontSize, JToggleButton(msg.mouseClickNukeLarge))
    val mouseDumpGenome = brush(fontSize, JToggleButton(msg.mouseClickPrintGenome))
    val mousePutMonster = brush(fontSize, JToggleButton(msg.mouseClickAddMonster))
    
    mousePutMonster.setEnabled(compatibleMonster.nonEmpty)

    val clickCommands = LinkedBlockingDeque[(Field, Field.StepStatistics) => Unit]()
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
        clickCommands.addLast((e, _) => e.findAndMarkLongestGenome(lastMagentaLabel))
    }))
    val magentaMostProductive = brush(fontSize, JToggleButton(new AbstractAction(msg.highlightMaxChildren) {
      override def actionPerformed(e: ActionEvent): Unit =
        lastMagentaLabel += 1
        view.setMagentaLabel(lastMagentaLabel)
        clickCommands.addLast((e, _) => e.findAndMarkMostProductive(lastMagentaLabel))
    }))
    val magentaFastest = brush(fontSize, JToggleButton(new AbstractAction(msg.highlightFastest) {
      override def actionPerformed(e: ActionEvent): Unit =
        lastMagentaLabel += 1
        view.setMagentaLabel(lastMagentaLabel)
        clickCommands.addLast((e, _) => e.findAndMarkFastest(lastMagentaLabel))
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
    
    def executePause(newPaused: Boolean): Unit =
      paused.set(newPaused)
      pauseButton.setText(if newPaused then msg.resume else msg.pause) // the next action of the button is inverted
    
    pauseButton.setBackground(Color.BLUE.darker().darker())
    pauseButton.setForeground(Color.WHITE)
    pauseButton.setFont(pauseButton.getFont.deriveFont(fontSize.toFloat * 2))
    pauseButton.setAlignmentX(Component.LEFT_ALIGNMENT)
    pauseButton.addActionListener: ev =>
      executePause(!paused.get())
    
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
      executePause(false)

    rightPane.add(restartButton)

    view.addMouseListener(new MouseAdapter {
      override def mouseClicked(e: MouseEvent): Unit =
        val (x, y) = view.translate(e.getX, e.getY)
        
        // FAT WARNING HERE
        // All these actions change the state of the field (including the RNG)
        // We need to deal with it once we support replays, either by disabling this or by logging this.

        if mouseSmallFood.isSelected then clickCommands.addLast((e, s) => e.increaseFood(x, y, smallRadius, s.maxFood * 1.05))
        if mouseLargeFood.isSelected then clickCommands.addLast((e, s) => e.increaseFood(x, y, largeRadius, s.maxFood * 1.05))
        if mouseSmallDestroy.isSelected then clickCommands.addLast((e, _) => e.eraseEverything(x, y, smallRadius))
        if mouseLargeDestroy.isSelected then clickCommands.addLast((e, _) => e.eraseEverything(x, y, largeRadius))
        if mouseDumpGenome.isSelected then findAndDumpIndividual(field, x, y, 0)
        if mousePutMonster.isSelected then clickCommands.addLast: (e, _) =>
          val cell = e.getCell(x, y)
          val monster = Individual(compatibleMonster.get, -1)
          cell.setIndividual(monster, config.random.nextInt(4), initialHealth)
    })

    val window = JFrame(msg.title)
    window.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    window.setLayout(BorderLayout())
    window.add(view, BorderLayout.CENTER)
    if enableLegend then window.add(rightPane, if (legendIsOnRight) BorderLayout.LINE_END else BorderLayout.LINE_START)
    window.setExtendedState(Frame.MAXIMIZED_BOTH)
    window.setUndecorated(true)
    window.setVisible(true)

    if useSound then
      val soundThread = Thread(new SoundWriterJob(field, DefaultSynthesizer, soundFrequency), "Sound thread")
      soundThread.setDaemon(true)
      soundThread.start()

    @tailrec
    def work(generation0: Int): Unit = if window.isVisible then
      val nextGenerationNo = if restarted.getAndSet(false) then

        // FAT WARNING HERE
        // The reset does not reset the RNG to its defaults, so the new run will be different even if we fix the seed.
        // We need to deal with it once we support replays, either by disabling this or by logging this.
        
        field.initialize(config)
        1
      else generation0 + 1

      val actionStatistics = field.simulationStep(config, nextGenerationNo)
      view.fetchField()
      
      drainClickQueue(clickCommands, field, actionStatistics)
      SwingEx.invokeLater:
        statTime.setValue(nextGenerationNo.toString)
        statNBacteria.setValue(actionStatistics.numberOfBacteria.toString)
        statMaxGenome.setValue(actionStatistics.maxGenomeSize.toString)

        actionsEat.setValue(actionStatistics.nEats.toString)
        actionsMove.setValue(actionStatistics.nMoves.toString)
        actionsFork.setValue(actionStatistics.nForks.toString)
        actionsCW.setValue(actionStatistics.nClockwise.toString)
        actionsCCW.setValue(actionStatistics.nCounterClockwise.toString)

        statNMonsters.setValue(actionStatistics.nMonsters.toString)
        statAverageHealth.setValue(String.format(Locale.US, "%.2f", actionStatistics.averageHealth))
        statAverageGenome.setValue(String.format(Locale.US, "%.2f", actionStatistics.averageGenomeSize))
        statMaxHealth.setValue(String.format(Locale.US, "%.2f", actionStatistics.maximalHealth))
        statSumEnergy.setValue(String.format(Locale.US, "%.2f", actionStatistics.totalFood))
        statMaxSpeed.setValue(String.format(Locale.US, "%.2f", actionStatistics.maxSpeed))
        statMaxChildren.setValue(actionStatistics.maxChildren.toString)
        statMaxDistance.setValue(actionStatistics.maxTravelDistance.toString)
        statMaxLifeSpan.setValue(actionStatistics.maxLife.toString)
      
      if autoPause > 0 && nextGenerationNo % autoPause == 0 then
        SwingEx.invokeAndWait:
          executePause(true)
      
      while paused.get() && window.isVisible do Thread.sleep(100)
      work(nextGenerationNo)
    end work
    
    work(0)
  end main
