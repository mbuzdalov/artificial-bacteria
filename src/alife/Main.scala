package alife

import java.awt.event.{ActionEvent, MouseAdapter, MouseEvent}
import java.awt.image.BufferedImage
import java.awt._
import java.io.FileReader
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{LinkedBlockingDeque, ThreadLocalRandom}
import java.util.{Locale, Properties}
import javax.swing._
import scala.annotation.tailrec

/**
 * A first attempt to run the bacteria system
 */
object Main {
  private def loadProperties(args: Array[String]): Properties = {
    val properties = new Properties()
    val fileName = if (args.length == 0) "config.properties" else args(0)
    val propReader = new FileReader(fileName)
    properties.load(propReader)
    propReader.close()
    properties
  }

  private def brush(fontSize: Int, label: JLabel): JLabel = {
    label.setForeground(Color.WHITE)
    label.setFont(label.getFont.deriveFont(fontSize.toFloat))
    label.setAlignmentX(Component.LEFT_ALIGNMENT)
    label
  }

  private def wellAlignedBox(textWidth: Int, fontSize: Int): Component = {
    val dim = new Dimension(textWidth, fontSize)
    val rv = new Box.Filler(dim, dim, dim)
    rv.setAlignmentX(Component.LEFT_ALIGNMENT)
    rv
  }

  private def makeMonotoneIcon(size: Int, color: Color): ImageIcon = {
    val image = new BufferedImage(size, size, BufferedImage.TYPE_INT_ARGB)
    val rgb = color.getRGB
    for (x <- 0 until size; y <- 0 until size) image.setRGB(x, y, rgb)
    new ImageIcon(image)
  }

  private class StatText(fontSize: Int, prefix: String) extends JLabel {
    brush(fontSize, this)
    def setValue(suffix: String): Unit = setText(prefix + suffix)
  }

  @tailrec
  private def findAndDumpIndividual(field: Field, x: Int, y: Int, d: Int): Unit = {
    if (d == 11) {
      println("Поблизости никого нет")
    } else {
      (-d to d).view.flatMap(dx => Seq(
        Option(field.getIndividual(x + dx, y + d - math.abs(dx))),
        Option(field.getIndividual(x + dx, y - d + math.abs(dx))),
      ).flatten).headOption match {
        case Some(g) => println(g.genome)
        case None => findAndDumpIndividual(field, x, y, d + 1)
      }
    }
  }

  @tailrec
  private def drainClickQueue(queue: LinkedBlockingDeque[(Field, Field.StepStatistics) => Unit],
                              field: Field, stats: Field.StepStatistics): Unit = {
    val next = queue.pollFirst()
    if (next != null) {
      next.apply(field, stats)
      drainClickQueue(queue, field, stats)
    }
  }

  private def makeMonster(): Individual = Individual(Monsters.First, -1)

  private def initializeFieldRandomly(field: Field, initialBacteriaProbability: Double,
                                      initialGenomeLength: Int, initialHealth: Double): Unit = {
    var y = 0
    while (y < field.height) {
      var x = 0
      while (x < field.width) {
        field.setDebris(x, y, 0)
        field.setEnergy(x, y, 1e-9)
        if (ThreadLocalRandom.current().nextDouble( ) < initialBacteriaProbability) {
          field.setIndividual(x, y,
            Individual(IndexedSeq.tabulate(initialGenomeLength)(Instruction.random), 0),
            ThreadLocalRandom.current().nextInt(4),
            initialHealth)
        } else {
          field.setIndividual(x, y, null, 0, 0)
        }
        x += 1
      }
      y += 1
    }
  }

  def main(args: Array[String]): Unit = {
    System.setProperty("awt.useSystemAAFontSettings", "on")
    System.setProperty("swing.aatext", "true")
    val properties = loadProperties(args)

    val constants = Field.Constants(
      rotationCost = properties.getProperty("rotationCost").toDouble,
      moveCost = properties.getProperty("moveCost").toDouble,
      eatCost = properties.getProperty("eatCost").toDouble,
      forkCost = properties.getProperty("forkCost").toDouble,
      debrisDegradation = properties.getProperty("debrisDegradation").toDouble,
      debrisToEnergy = properties.getProperty("debrisToEnergy").toDouble,
      synthesisInit = properties.getProperty("synthesisInit").toDouble,
      synthesisFinal = properties.getProperty("synthesisFinal").toDouble,
      synthesisDecay = properties.getProperty("synthesisDecay").toDouble,
      idleCost = properties.getProperty("idleCost").toDouble,
      healthMultiple = properties.getProperty("healthMultiple").toDouble,
      spotPeriodX = properties.getProperty("spotPeriodX").toDouble,
      spotPeriodY = properties.getProperty("spotPeriodY").toDouble,
      spotSpeedX = properties.getProperty("spotSpeedX").toDouble,
      spotSpeedY = properties.getProperty("spotSpeedY").toDouble,
      spotDecay = properties.getProperty("spotDecay").toDouble
    )

    val textWidth = properties.getProperty("textWidth").toInt
    val fontSize = properties.getProperty("fontSize").toInt

    val width = properties.getProperty("fieldWidth").toInt
    val height = properties.getProperty("fieldHeight").toInt
    val pixelScale = properties.getProperty("pixelScale").toInt
    val initialHealth = properties.getProperty("initialHealth").toDouble
    val initialGenomeLength = properties.getProperty("initialGenomeLength").toInt
    val initialBacteriaProbability = properties.getProperty("initialBacteriaProbability").toDouble
    val field = new Field(width, height)

    val smallRadius = properties.getProperty("smallRadius").toInt
    val largeRadius = properties.getProperty("largeRadius").toInt
    val enableGenomeDumping = properties.getProperty("enableGenomeDumping").toBoolean
    val legendIsOnRight = properties.getProperty("legendIsOnRight").toBoolean

    initializeFieldRandomly(field, initialBacteriaProbability, initialGenomeLength, initialHealth)
    val view = new FieldVisualizer(field, pixelScale)
    view.setBackground(Color.BLACK)

    val rightPane = new JPanel()
    rightPane.setLayout(new BoxLayout(rightPane, BoxLayout.PAGE_AXIS))
    rightPane.setBackground(Color.BLACK)
    rightPane.setBorder(BorderFactory.createLineBorder(Color.BLACK, 10))

    rightPane.add(brush(fontSize, new JLabel("Легенда")))
    rightPane.add(brush(fontSize, new JLabel("Бактерии", makeMonotoneIcon(fontSize, Color.RED), SwingConstants.LEADING)))
    rightPane.add(brush(fontSize, new JLabel("Еда", makeMonotoneIcon(fontSize, Color.GREEN), SwingConstants.LEADING)))
    rightPane.add(brush(fontSize, new JLabel("Отходы и останки", makeMonotoneIcon(fontSize, Color.BLUE), SwingConstants.LEADING)))
    rightPane.add(brush(fontSize, new JLabel("Выделенный штамм", makeMonotoneIcon(fontSize, Color.MAGENTA), SwingConstants.LEADING)))
    rightPane.add(wellAlignedBox(textWidth, fontSize))

    val statTime = new StatText(fontSize, "Время: ")
    val statNBacteria = new StatText(fontSize, "Живых бактерий: ")
    val statNMonsters = new StatText(fontSize, "Из них потомков монстров: ")
    val statAverageHealth = new StatText(fontSize, "Среднее здоровье: ")
    val statSumEnergy = new StatText(fontSize, "Количество еды: ")

    rightPane.add(brush(fontSize, new JLabel("Статистика:")))
    rightPane.add(statTime)
    rightPane.add(statNBacteria)
    rightPane.add(statNMonsters)
    rightPane.add(statAverageHealth)
    rightPane.add(statSumEnergy)
    rightPane.add(wellAlignedBox(textWidth, fontSize))

    val statMaxLifeSpan = new StatText(fontSize, "Максимальный срок жизни: ")
    val statMaxHealth = new StatText(fontSize, "Максимальное здоровье: ")
    val statGenome = new StatText(fontSize, "Максимальный размер генома: ")
    val statMaxChildren = new StatText(fontSize, "Максимальное число детей: ")
    val statMaxDistance = new StatText(fontSize, "Самый длинный путь: ")
    val statMaxSpeed = new StatText(fontSize, "Самая большая скорость: ")

    rightPane.add(brush(fontSize, new JLabel("Самые-самые:")))
    rightPane.add(statMaxHealth)
    rightPane.add(statMaxLifeSpan)
    rightPane.add(statGenome)
    rightPane.add(statMaxChildren)
    rightPane.add(statMaxDistance)
    rightPane.add(statMaxSpeed)
    rightPane.add(wellAlignedBox(textWidth, fontSize))

    val actionsEat = new StatText(fontSize, "Питаться: ")
    val actionsMove = new StatText(fontSize, "Двигаться: ")
    val actionsFork = new StatText(fontSize, "Делиться: ")
    val actionsCW = new StatText(fontSize, "Вращаться (по часовой): ")
    val actionsCCW = new StatText(fontSize, "Вращаться (против часовой): ")

    rightPane.add(brush(fontSize, new JLabel("Число действий:")))
    rightPane.add(actionsEat)
    rightPane.add(actionsMove)
    rightPane.add(actionsFork)
    rightPane.add(actionsCW)
    rightPane.add(actionsCCW)
    rightPane.add(wellAlignedBox(textWidth, fontSize))

    val mouseDoNothing = new JToggleButton("Ничего не делать")
    val mouseSmallFood = new JToggleButton("Добавить еды (малый радиус)")
    val mouseLargeFood = new JToggleButton("Добавить еды (большой радиус")
    val mouseSmallDestroy = new JToggleButton("Все стереть (малый радиус)")
    val mouseLargeDestroy = new JToggleButton("Все стереть (большой радиус)")
    val mouseDumpGenome = new JToggleButton("Вывести геном на консоль")
    val mousePutMonster = new JToggleButton("Добавить монстра")

    val clickCommands = new LinkedBlockingDeque[(Field, Field.StepStatistics) => Unit]()
    val mouseClickGroup = new ButtonGroup
    mouseClickGroup.add(mouseDoNothing)
    mouseClickGroup.add(mouseSmallFood)
    mouseClickGroup.add(mouseLargeFood)
    mouseClickGroup.add(mouseSmallDestroy)
    mouseClickGroup.add(mouseLargeDestroy)
    mouseClickGroup.add(mouseDumpGenome)
    mouseClickGroup.add(mousePutMonster)
    mouseDoNothing.setSelected(true)

    rightPane.add(brush(fontSize, new JLabel("Действие клика мыши:")))
    rightPane.add(mouseDoNothing)
    rightPane.add(mouseSmallFood)
    rightPane.add(mouseLargeFood)
    rightPane.add(mouseSmallDestroy)
    rightPane.add(mouseLargeDestroy)
    if (enableGenomeDumping) rightPane.add(mouseDumpGenome)
    rightPane.add(mousePutMonster)
    rightPane.add(wellAlignedBox(textWidth, fontSize))

    var lastMagentaLabel = 0

    val magentaNone = new JToggleButton(new AbstractAction("Не выделять") {
      override def actionPerformed(e: ActionEvent): Unit = view.setMagentaLabel(-2)
    })
    val magentaMonster = new JToggleButton(new AbstractAction("Монстры и их потомки") {
      override def actionPerformed(e: ActionEvent): Unit = view.setMagentaLabel(-1)
    })
    val magentaLongestGenome = new JToggleButton(new AbstractAction("Найти самый длинный геном") {
      override def actionPerformed(e: ActionEvent): Unit = {
        lastMagentaLabel += 1
        view.setMagentaLabel(lastMagentaLabel)
        clickCommands.addLast((e, _) => e.findAndMarkLongestGenome(lastMagentaLabel))
      }
    })
    val magentaMostProductive = new JToggleButton(new AbstractAction("Максимальное число детей") {
      override def actionPerformed(e: ActionEvent): Unit = {
        lastMagentaLabel += 1
        view.setMagentaLabel(lastMagentaLabel)
        clickCommands.addLast((e, _) => e.findAndMarkMostProductive(lastMagentaLabel))
      }
    })
    val magentaFastest = new JToggleButton(new AbstractAction("Самая быстрая бактерия") {
      override def actionPerformed(e: ActionEvent): Unit = {
        lastMagentaLabel += 1
        view.setMagentaLabel(lastMagentaLabel)
        clickCommands.addLast((e, _) => e.findAndMarkFastest(lastMagentaLabel))
      }
    })

    val magentaGroup = new ButtonGroup
    magentaGroup.add(magentaNone)
    magentaGroup.add(magentaMonster)
    magentaGroup.add(magentaLongestGenome)
    magentaGroup.add(magentaMostProductive)
    magentaGroup.add(magentaFastest)
    magentaMonster.setSelected(true)

    rightPane.add(brush(fontSize, new JLabel("Выбор выделенного штамма:")))
    rightPane.add(magentaNone)
    rightPane.add(magentaMonster)
    rightPane.add(magentaLongestGenome)
    rightPane.add(magentaMostProductive)
    rightPane.add(magentaFastest)
    rightPane.add(Box.createVerticalGlue())

    val restartButton = new JButton("НАЧАТЬ ЗАНОВО")
    val restarted = new AtomicBoolean(false)
    restartButton.setBackground(Color.RED.darker().darker())
    restartButton.setForeground(Color.WHITE)
    restartButton.setFont(restartButton.getFont.deriveFont(fontSize.toFloat * 2))
    restartButton.setAlignmentX(Component.LEFT_ALIGNMENT)
    restartButton.addActionListener(_ => {
      lastMagentaLabel = 0
      mouseDoNothing.setSelected(true)
      magentaMonster.setSelected(true)
      restarted.set(true)
    })

    rightPane.add(restartButton)

    view.addMouseListener(new MouseAdapter {
      override def mouseClicked(e: MouseEvent): Unit = {
        val (x, y) = view.translate(e.getX, e.getY)

        if (mouseSmallFood.isSelected) clickCommands.addLast((e, s) => e.increaseEnergy(x, y, smallRadius, s.maxEnergy))
        if (mouseLargeFood.isSelected) clickCommands.addLast((e, s) => e.increaseEnergy(x, y, largeRadius, s.maxEnergy))
        if (mouseSmallDestroy.isSelected) clickCommands.addLast((e, _) => e.eraseEverything(x, y, smallRadius))
        if (mouseLargeDestroy.isSelected) clickCommands.addLast((e, _) => e.eraseEverything(x, y, largeRadius))
        if (mouseDumpGenome.isSelected) findAndDumpIndividual(field, x, y, 0)
        if (mousePutMonster.isSelected) clickCommands.addLast((e, _) => e.setIndividual(x, y, makeMonster(), ThreadLocalRandom.current().nextInt(4), initialHealth))
      }
    })

    val window = new JFrame("Бактерии")
    window.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    window.setLayout(new BorderLayout())
    window.add(view, BorderLayout.CENTER)
    window.add(rightPane, if (legendIsOnRight) BorderLayout.LINE_END else BorderLayout.LINE_START)
    window.setExtendedState(Frame.MAXIMIZED_BOTH)
    window.setUndecorated(true)
    window.setVisible(true)

    @tailrec
    def work(generation0: Int): Unit = if (window.isVisible) {
      val generation = if (restarted.getAndSet(false)) {
        initializeFieldRandomly(field, initialBacteriaProbability, initialGenomeLength, initialHealth)
        0
      } else generation0

      view.fetchField()
      val nBacteria = field.getNumberOfBacteria
      val genomeSize = field.getMaxGenomeSize

      SwingUtilities.invokeLater(() => {
        statTime.setValue(generation.toString)
        statNBacteria.setValue(nBacteria.toString)
        statGenome.setValue(genomeSize.toString)
      })

      if (field.getNumberOfBacteria > 0) {
        val actionStatistics = field.simulationStep(constants, generation)
        drainClickQueue(clickCommands, field, actionStatistics)
        SwingUtilities.invokeLater(() => {
          actionsEat.setValue(actionStatistics.nEats.toString)
          actionsMove.setValue(actionStatistics.nMoves.toString)
          actionsFork.setValue(actionStatistics.nForks.toString)
          actionsCW.setValue(actionStatistics.nClockwise.toString)
          actionsCCW.setValue(actionStatistics.nCounterClockwise.toString)

          statNMonsters.setValue(actionStatistics.nMonsters.toString)
          statAverageHealth.setValue(String.format(Locale.US, "%.2f", actionStatistics.averageHealth))
          statMaxHealth.setValue(String.format(Locale.US, "%.2f", actionStatistics.maximalHealth))
          statSumEnergy.setValue(String.format(Locale.US, "%.2f", actionStatistics.totalEnergy))
          statMaxSpeed.setValue(String.format(Locale.US, "%.2f", actionStatistics.maxSpeed))
          statMaxChildren.setValue(actionStatistics.maxChildren.toString)
          statMaxDistance.setValue(actionStatistics.maxTravelDistance.toString)
          statMaxLifeSpan.setValue(actionStatistics.maxLife.toString)
        })
        work(generation + 1)
      }
    }
    work(0)
  }
}
