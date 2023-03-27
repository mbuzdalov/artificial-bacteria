package alife

trait Messages {
  def title: String
  def restart: String

  def legend: String
  def legendForBacteria: String
  def legendForFood: String
  def legendForJunk: String
  def legendForSelection: String

  def stats: String
  def statsCountAlive: String
  def statsTimePassed: String
  def statsCountMonsters: String
  def statsAvgHealth: String
  def statsFood: String

  def best: String
  def bestLifeSpan: String
  def bestHealth: String
  def bestGenomeSize: String
  def bestChildren: String
  def bestDistance: String
  def bestSpeed: String

  def nActions: String
  def nActionsFeed: String
  def nActionsMove: String
  def nActionsFork: String
  def nActionsCW: String
  def nActionsCCW: String

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
}

object Messages {
  def apply(lang: String): Messages = if (lang == "ru") Russian else English

  object English extends Messages {
    def title: String = "Artificial Bacteria"
    def restart: String = "BEGIN ANEW"

    def legend: String = "Legend:"
    def legendForBacteria: String = "Bacteria"
    def legendForFood: String = "Food"
    def legendForJunk: String = "Junk and remnants"
    def legendForSelection: String = "Highlighted ones"

    def stats: String = "Statistics:"
    def statsCountAlive: String = "Alive bacteria: "
    def statsTimePassed: String = "Time passed: "
    def statsCountMonsters: String = "Of which monsters: "
    def statsAvgHealth: String = "Average health: "
    def statsFood: String = "Amount of food: "

    def best: String = "The best ones:"
    def bestLifeSpan: String = "Max life span: "
    def bestHealth: String = "Max health: "
    def bestGenomeSize: String = "Max genome size: "
    def bestChildren: String = "Max no of children: "
    def bestDistance: String = "The longest path: "
    def bestSpeed: String = "The bigged speed: "

    def nActions: String = "Total number of actions:"
    def nActionsFeed: String = "Feed: "
    def nActionsMove: String = "Move: "
    def nActionsFork: String = "Fork: "
    def nActionsCW: String = "Rotate (CW): "
    def nActionsCCW: String = "Rotate (CCW): "

    def mouseClick: String = "Mouse click action:"
    def mouseClickNothing: String = "Do nothing"
    def mouseClickFoodSmall: String = "Add food (small radius)"
    def mouseClickFoodLarge: String = "Add food (large radius)"
    def mouseClickNukeSmall: String = "Nuke'em all (small radius)"
    def mouseClickNukeLarge: String = "Nuke'em all (large radius)"
    def mouseClickPrintGenome: String = "Print genome to console"
    def mouseClickAddMonster: String = "Add a monster"

    def highlight: String = "Who to highlight:"
    def highlightNothing: String = "Nobody"
    def highlightMonsters: String = "Monsters and their children"
    def highlightLongest: String = "The longest genome"
    def highlightMaxChildren: String = "Max number of children"
    def highlightFastest: String = "The fastest bacterium"
  }

  object Russian extends Messages {
    def title: String = "Бактерии"
    def restart: String = "НАЧАТЬ ЗАНОВО"

    def legend: String = "Обозначения:"
    def legendForBacteria: String = "Бактерии"
    def legendForFood: String = "Еда"
    def legendForJunk: String = "Отходы и останки"
    def legendForSelection: String = "Выделенный штамм"

    def stats: String = "Статистика:"
    def statsCountAlive: String = "Живых бактерий: "
    def statsTimePassed: String = "Время: "
    def statsCountMonsters: String = "Из них потомков монстров: "
    def statsAvgHealth: String = "Среднее здоровье: "
    def statsFood: String = "Количество еды: "

    def best: String = "Самые-самые: "
    def bestLifeSpan: String = "Максимальный срок жизни: "
    def bestHealth: String = "Максимальное здоровье: "
    def bestGenomeSize: String = "Максимальный размер генома: "
    def bestChildren: String = "Максимальное число детей: "
    def bestDistance: String = "Самый длинный путь: "
    def bestSpeed: String = "Самая большая скорость: "

    def nActions: String = "Общее число действий:"
    def nActionsFeed: String = "Питаться: "
    def nActionsMove: String = "Двигаться: "
    def nActionsFork: String = "Делиться: "
    def nActionsCW: String = "Вращаться (по часовой): "
    def nActionsCCW: String = "Вращаться (против часовой): "

    def mouseClick: String = "Действие клика мыши:"
    def mouseClickNothing: String = "Ничего не делать"
    def mouseClickFoodSmall: String = "Добавить еды (малый радиус)"
    def mouseClickFoodLarge: String = "Добавить еды (большой радиус)"
    def mouseClickNukeSmall: String = "Все стереть (малый радиус)"
    def mouseClickNukeLarge: String = "Все стереть (большой радиус)"
    def mouseClickPrintGenome: String = "Вывести геном на консоль"
    def mouseClickAddMonster: String = "Добавить монстра"

    def highlight: String = "Выбор выделенного штамма:"
    def highlightNothing: String = "Не выделять"
    def highlightMonsters: String = "Монстры и их потомки"
    def highlightLongest: String = "Найти самый длинный геном"
    def highlightMaxChildren: String = "Максимальное число потомков"
    def highlightFastest: String = "Самая быстрая бактерия"
  }
}
