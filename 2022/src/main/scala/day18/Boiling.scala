package day18

import utils.Base

import scala.annotation.tailrec
case class Cube(x: Int, y: Int, z: Int):
  def neighbors: Set[Cube] =
    Set(-1, 1).flatMap(delta => Seq(addX, addY, addZ).map(_(delta)))

  private def addX(dx: Int): Cube = Cube(x + dx, y, z)
  private def addY(dy: Int): Cube = Cube(x, y + dy, z)
  private def addZ(dz: Int): Cube = Cube(x, y, z + dz)

object Boiling extends Base:

  private val cubes: Set[Cube] =
    input.toLines.map { l =>
      val Array(x, y, z) = l.split(",").map(_.toInt): @unchecked
      Cube(x, y, z)
    }.toSet

  private val xs: Set[Int] = cubes.map(_.x)
  private val ys: Set[Int] = cubes.map(_.y)
  private val zs: Set[Int] = cubes.map(_.z)

  private def getSurfaceArea(cubes: Set[Cube]): Int =
    val cubeNeighbors =
      cubes.map(c => c -> c.neighbors.intersect(cubes)).toMap
    cubeNeighbors.values.map(ns => 6 - ns.size).sum

  private def allCubes: Set[Cube] =
    (xs.min - 1 to xs.max + 1).flatMap { x =>
      (ys.min - 1 to ys.max + 1).flatMap { y =>
        (zs.min - 1 to zs.max + 1).map { z =>
          Cube(x, y, z)
        }
      }
    }.toSet

  private val emptySpaces: Set[Cube] = allCubes -- cubes

  private val emptyNeighbors: Map[Cube, Set[Cube]] =
    emptySpaces.map(c => c -> c.neighbors.intersect(emptySpaces)).toMap

  private def groupCubes(cubeNeighbors: Map[Cube, Set[Cube]]): Set[Set[Cube]] =
    @tailrec
    def findParent(parents: Map[Cube, Cube], cube: Cube): Cube =
      if parents(cube) == cube then cube
      else findParent(parents, parents(cube))

    val cubeParents = cubeNeighbors.keys.map(c => c -> c).toMap

    val parents = cubeNeighbors
      .foldLeft(cubeParents) { case (parents, (cube, neighbors)) =>
        neighbors.foldLeft(parents) { case (parents, neighbor) =>
          val cubeRoot = findParent(parents, cube)
          val neighborRoot = findParent(parents, neighbor)

          if cubeRoot != neighborRoot then
            parents
              .updated(neighborRoot, cubeRoot)
              .updated(cube, cubeRoot)
              .updated(neighbor, cubeRoot)
          else
            parents
              .updated(cube, cubeRoot)
              .updated(neighbor, cubeRoot)
        }
      }

    cubeNeighbors.keys.groupBy(findParent(parents, _)).values.toSet.map(_.toSet)

  private val initialSurfaceArea: Int = getSurfaceArea(cubes)

  private val internalSpaces: Set[Set[Cube]] =
    groupCubes(emptyNeighbors).filterNot(
      _.contains(Cube(xs.max + 1, ys.max + 1, zs.max + 1))
    )

  private val internalSurfaceArea: Int =
    internalSpaces.toSeq.map(getSurfaceArea).sum

  override def part1: Any = initialSurfaceArea
  override def part2: Any = initialSurfaceArea - internalSurfaceArea

  @main def main(): Unit = run()
