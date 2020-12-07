package alife

import java.awt.event.{MouseAdapter, MouseEvent}
import java.awt.image.BufferedImage
import java.awt._
import java.io.FileReader
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
  private def drainClickQueue(queue: LinkedBlockingDeque[(Field, Field.StepStatistics) => Unit],
                              field: Field, stats: Field.StepStatistics): Unit = {
    val next = queue.pollFirst()
    if (next != null) {
      next.apply(field, stats)
      drainClickQueue(queue, field, stats)
    }
  }

  def main(args: Array[String]) {
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

    var y = 0
    while (y < height) {
      var x = 0
      while (x < width) {
        if (ThreadLocalRandom.current().nextDouble( ) < initialBacteriaProbability) {
          field.setIndividual(x, y,
            Individual(IndexedSeq.tabulate(initialGenomeLength)(Instruction.random), 0),
            ThreadLocalRandom.current().nextInt(4),
            initialHealth)
        }
        x += 1
      }
      y += 1
    }

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
    rightPane.add(Box.createRigidArea(new Dimension(textWidth, fontSize)))

    val statTime = new StatText(fontSize, "Время: ")
    val statNBacteria = new StatText(fontSize, "Живых бактерий: ")
    val statAverageHealth = new StatText(fontSize, "Среднее здоровье: ")
    val statSumEnergy = new StatText(fontSize, "Количество еды: ")

    rightPane.add(brush(fontSize, new JLabel("Статистика:")))
    rightPane.add(statTime)
    rightPane.add(statNBacteria)
    rightPane.add(statAverageHealth)
    rightPane.add(statSumEnergy)
    rightPane.add(Box.createRigidArea(new Dimension(textWidth, fontSize)))

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
    rightPane.add(Box.createRigidArea(new Dimension(textWidth, fontSize)))

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
    rightPane.add(Box.createRigidArea(new Dimension(textWidth, fontSize)))

    val mouseDoNothing = new JToggleButton("Ничего не делать")
    val mouseSmallFood = new JToggleButton("Добавить еды (малый радиус)")
    val mouseLargeFood = new JToggleButton("Добавить еды (большой радиус")
    val mouseSmallDestroy = new JToggleButton("Все стереть (малый радиус)")
    val mouseLargeDestroy = new JToggleButton("Все стереть (большой радиус)")

    val clickCommands = new LinkedBlockingDeque[(Field, Field.StepStatistics) => Unit]()
    val mouseClickGroup = new ButtonGroup
    mouseClickGroup.add(mouseDoNothing)
    mouseClickGroup.add(mouseSmallFood)
    mouseClickGroup.add(mouseLargeFood)
    mouseClickGroup.add(mouseSmallDestroy)
    mouseClickGroup.add(mouseLargeDestroy)
    mouseDoNothing.setSelected(true)

    rightPane.add(brush(fontSize, new JLabel("Действие клика мыши:")))
    rightPane.add(mouseDoNothing)
    rightPane.add(mouseSmallFood)
    rightPane.add(mouseLargeFood)
    rightPane.add(mouseSmallDestroy)
    rightPane.add(mouseLargeDestroy)

    view.addMouseListener(new MouseAdapter {
      override def mouseClicked(e: MouseEvent): Unit = {
        val (x, y) = view.translate(e.getX, e.getY)

        if (mouseSmallFood.isSelected) clickCommands.addLast((e, s) => e.increaseEnergy(x, y, smallRadius, s.maxEnergy))
        if (mouseLargeFood.isSelected) clickCommands.addLast((e, s) => e.increaseEnergy(x, y, largeRadius, s.maxEnergy))
        if (mouseSmallDestroy.isSelected) clickCommands.addLast((e, s) => e.eraseEverything(x, y, smallRadius))
        if (mouseLargeDestroy.isSelected) clickCommands.addLast((e, s) => e.eraseEverything(x, y, largeRadius))
      }
    })

    val window = new JFrame("Бактерии")
    window.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    window.setLayout(new BorderLayout())
    window.add(view, BorderLayout.CENTER)
    window.add(rightPane, BorderLayout.LINE_END)
    window.setExtendedState(Frame.MAXIMIZED_BOTH)
    window.setUndecorated(true)
    window.setVisible(true)

    @tailrec
    def work(generation: Int): Unit = if (window.isVisible) {
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
