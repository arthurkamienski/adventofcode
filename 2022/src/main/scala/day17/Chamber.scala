package day17

import scala.annotation.tailrec

object Chamber {
  val width: Int = 7
  private val bottomRow: Set[Coord] = (0 to 6).map(x => Coord(x, -1)).toSet

  def apply(
      shapes: Iterator[Coord => Shape],
      jets: Iterator[Int]
  ): Chamber =
    Chamber(None, -1, bottomRow, shapes, jets, Seq(), "")
}

case class Chamber(
    currShape: Option[Shape],
    highestPoint: Int,
    rocks: Set[Coord],
    shapeGenerator: Iterator[Coord => Shape],
    jetGenerator: Iterator[Int],
    shapes: Seq[Shape],
    shapesString: String
):
  private def startPosition: Coord = Coord(2, highestPoint + 4)

  private def addShape(): Chamber = {
    Chamber(
      currShape = Some(shapeGenerator.next()(startPosition)),
      highestPoint,
      rocks,
      shapeGenerator,
      jetGenerator,
      shapes,
      shapesString
    )
  }

  private def isShapeStopped(shape: Shape): Boolean =
    shape.bottomCoords.map(_.add(0, -1)).intersect(rocks).nonEmpty

  private def nextState: Chamber = currShape match {
    case None => addShape()
    case Some(shape) =>
      val jet = jetGenerator.next()
      val blownShape = {
        val newShape = shape.blow(jet)
        val hasCollision = newShape.allCoords
          .intersect(rocks)
          .nonEmpty
        if newShape.left == -1 || newShape.right == 7 || hasCollision
        then shape
        else newShape
      }

      if isShapeStopped(blownShape) then
        Chamber(
          currShape = None,
          highestPoint.max(blownShape.top),
          rocks = rocks ++ blownShape.allCoords,
          shapeGenerator,
          jetGenerator,
          blownShape +: shapes,
          shapesString + blownShape.toPositionString
        )
      else
        Chamber(
          currShape = Some(blownShape.drop),
          highestPoint,
          rocks = rocks,
          shapeGenerator,
          jetGenerator,
          shapes,
          shapesString
        )
  }

  @tailrec
  private final def dropShapeToBottom: Chamber = currShape match {
    case None => this
    case _    => nextState.dropShapeToBottom
  }

  def dropNewShapeToBottom: Chamber = currShape match {
    case None => addShape().dropShapeToBottom
    case _    => nextState.dropShapeToBottom
  }

  def printMap(): Unit =
    val rockCoords = currShape match
      case None    => Set()
      case Some(s) => s.allCoords

    val highestPoint =
      (rocks.map(_.y).max + 4).max(rockCoords.map(_.y).maxOption.getOrElse(0))

    (-1 to highestPoint).reverse.foreach(y =>
      print('|')
      (0 to 6).foreach(x =>
        val c = Coord(x, y)
        if rocks.contains(c) then print('#')
        else if rockCoords.contains(c) then print('@')
        else print('.')
      )
      println('|')
    )

  private lazy val repeatingShapePattern: Option[String] =
    "(.+?)\\1+".r.findFirstMatchIn(shapesString) match
      case Some(m) => Some(m.group(1))
      case None    => None

  def heightAtNShapes(n: BigInt): Option[BigInt] = repeatingShapePattern match
    case Some(pattern) =>
      val patternLength = pattern.split("[0-9]").length
      val nShapesBeforePattern =
        shapesString.replace(pattern, "").split("[0-9]").length

      val nRepeats = (n - nShapesBeforePattern) / patternLength
      val remainingShapes = ((n - nShapesBeforePattern) % patternLength).toInt

      val orderedShapes = shapes.reverse

      val lastShapeBeforePattern = orderedShapes(nShapesBeforePattern - 1)
      val lastRemainingShape = orderedShapes(
        nShapesBeforePattern + remainingShapes - 1
      )
      val lastShapeInPattern = orderedShapes(
        nShapesBeforePattern + patternLength - 1
      )

      val heightBeforePattern = lastShapeBeforePattern.top + 1
      val patternHeight = lastShapeInPattern.top + 1 - heightBeforePattern
      val remainingHeight = lastRemainingShape.top + 1 - heightBeforePattern

      Some(
        heightBeforePattern + (patternHeight * nRepeats) + remainingHeight
      )
    case None => None
