package day17

trait Shape:
  val width: Int
  val height: Int
  val position: Coord
  val Coord(x, y) = position

  def top: Int = y + height - 1
  def right: Int = x + width - 1
  def left: Int = x
  def bottom: Int = y

  def allCoords: Set[Coord]
  def leftCoords: Set[Coord]
  def rightCoords: Set[Coord]
  def bottomCoords: Set[Coord]

  def generator: Coord => Shape

  def toPositionString: String

  def blow(dx: Int): Shape = generator(position.add(dx, 0))
  def drop: Shape = generator(position.add(0, -1))

case class HBar(position: Coord) extends Shape:
  override val width: Int = 4
  override val height: Int = 1

  override def allCoords: Set[Coord] =
    (0 to 3).map(dx => position.add(dx, 0)).toSet

  override def leftCoords: Set[Coord] = Set(position)
  override def rightCoords: Set[Coord] = Set(Coord(x + width, y))
  override def bottomCoords: Set[Coord] = allCoords

  override def generator: Coord => Shape = HBar.apply

  override def toPositionString: String = s"H$x"

case class VBar(position: Coord) extends Shape:
  override val width: Int = 1
  override val height: Int = 4

  override def allCoords: Set[Coord] =
    (0 to 3).map(dy => position.add(0, dy)).toSet

  override def leftCoords: Set[Coord] = allCoords
  override def rightCoords: Set[Coord] = allCoords
  override def bottomCoords: Set[Coord] = Set(position)

  override def generator: Coord => Shape = VBar.apply

  override def toPositionString: String = s"V$x"

case class Cube(position: Coord) extends Shape:
  override val width: Int = 2
  override val height: Int = 2

  override def allCoords: Set[Coord] =
    (0 to 1)
      .flatMap(dx => (0 to 1).map(dy => position.add(dx, dy)))
      .toSet

  override def leftCoords: Set[Coord] =
    (0 to 1).map(dy => position.add(0, dy)).toSet
  override def rightCoords: Set[Coord] =
    (0 to 1).map(dy => position.add(1, dy)).toSet
  override def bottomCoords: Set[Coord] =
    (0 to 1).map(dx => position.add(dx, 0)).toSet

  override def generator: Coord => Shape = Cube.apply
  override def toPositionString: String = s"C$x"

case class MirroredL(position: Coord) extends Shape:
  override val width: Int = 3
  override val height: Int = 3

  override def allCoords: Set[Coord] =
    (0 to 2)
      .map(dx => position.add(dx, 0))
      .toSet ++ (0 to 2).map(dy => position.add(2, dy)).toSet

  override def generator: Coord => Shape = MirroredL.apply

  override def leftCoords: Set[Coord] = Set(position)
  override def rightCoords: Set[Coord] =
    (0 to 2).map(dy => position.add(2, dy)).toSet
  override def bottomCoords: Set[Coord] =
    (0 to 2).map(dx => position.add(dx, 0)).toSet
  override def toPositionString: String = s"L$x"

case class Cross(position: Coord) extends Shape:
  override val width: Int = 3
  override val height: Int = 3

  override def allCoords: Set[Coord] =
    (0 to 2)
      .map(dx => position.add(dx, 1))
      .toSet ++ (0 to 2).map(dy => position.add(1, dy)).toSet

  override def leftCoords: Set[Coord] = Set(Coord(x, y + 1))
  override def rightCoords: Set[Coord] = Set(Coord(x + 2, y + 1))
  override def bottomCoords: Set[Coord] =
    Set(Coord(x + 1, y), Coord(x, y + 1), Coord(x + 2, y + 1))

  override def generator: Coord => Shape = Cross.apply
  override def toPositionString: String = s"X$x"
