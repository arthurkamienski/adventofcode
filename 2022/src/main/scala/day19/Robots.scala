package day19

import utils.Base

enum Robot:
  case Ore, Clay, Obsidian, Geode

case class Cost(ore: Int, clay: Int, obsidian: Int)

case class RobotCosts(ore: Cost, clay: Cost, obsidian: Cost, geode: Cost):
  def apply(robot: Robot): Cost = robot match
    case Robot.Ore      => ore
    case Robot.Clay     => clay
    case Robot.Obsidian => obsidian
    case Robot.Geode    => geode

case class Robots(ore: Int, clay: Int, obsidian: Int, geode: Int):
  def add(robot: Robot): Robots = robot match
    case Robot.Ore      => copy(ore = ore + 1)
    case Robot.Clay     => copy(clay = clay + 1)
    case Robot.Obsidian => copy(obsidian = obsidian + 1)
    case Robot.Geode    => copy(geode = geode + 1)

case class Resources(ore: Int, clay: Int, obsidian: Int, geode: Int):
  def canBuild(cost: Cost): Boolean =
    ore >= cost.ore && clay >= cost.clay && obsidian >= cost.obsidian

  def build(cost: Cost): Resources =
    Resources(
      ore - cost.ore,
      clay - cost.clay,
      obsidian - cost.obsidian,
      geode
    )

  def add(robots: Robots): Resources =
    Resources(
      ore + robots.ore,
      clay + robots.clay,
      obsidian + robots.obsidian,
      geode + robots.geode
    )

case class BluePrint(
    id: Int,
    robots: Robots,
    robotCosts: RobotCosts,
    resources: Resources,
    time: Int
):
  private def buildRobot(robot: Robot): Option[BluePrint] =
    if resources.canBuild(robotCosts(robot)) then
      Some(
        BluePrint(
          id,
          robots.add(robot),
          robotCosts,
          resources.build(robotCosts(robot)).add(robots),
          time + 1
        )
      )
    else None

  private def doNothing(): BluePrint =
    BluePrint(id, robots, robotCosts, resources.add(robots), time + 1)

  def getNextStates: Set[BluePrint] =
    buildRobot(Robot.Geode) match
      case Some(geodeRobot) => Set(geodeRobot)
      case None =>
        buildRobot(Robot.Obsidian) match
          case Some(obsidianRobot) => Set(obsidianRobot)
          case None =>
            Set(
              buildRobot(Robot.Ore),
              buildRobot(Robot.Clay),
              Some(doNothing())
            ).flatten

  def qualityLevel: Int = id * resources.geode

object Robots extends Base:
  private val blueprints = input.toLines.map { line =>
    val Array(idString, rule) = line.split(": ")
    val id = idString.split(" ").last.toInt

    val Array(oreRobot, clayRobot, obsidianRobot, geodeRobot) =
      rule.split("""\. """)

    val oreRobotCost = """(\d+)""".r.findAllIn(oreRobot).toSeq.head.toInt
    val clayRobotCost = """(\d+)""".r.findAllIn(clayRobot).toSeq.head.toInt

    val Array(obsidianRobotOreCost: String, obsidianRobotClayCost: String) =
      """(\d+)""".r.findAllIn(obsidianRobot).toArray: @unchecked
    val Array(geodeRobotOreCost: String, geodeRobotObsidianCost: String) =
      """(\d+)""".r.findAllIn(geodeRobot).toArray: @unchecked

    val robotCosts = RobotCosts(
      Cost(oreRobotCost, 0, 0),
      Cost(clayRobotCost, 0, 0),
      Cost(obsidianRobotOreCost.toInt, obsidianRobotClayCost.toInt, 0),
      Cost(geodeRobotOreCost.toInt, 0, geodeRobotObsidianCost.toInt)
    )

    BluePrint(
      id,
      Robots(1, 0, 0, 0),
      robotCosts,
      Resources(0, 0, 0, 0),
      0
    )
  }

  private def getMaxGeodes(time: Int, blueprints: Seq[BluePrint]): Seq[BluePrint] =
    blueprints.map(blueprint =>
      (1 to time)
        .foldLeft(Set(blueprint)) { (blueprints, _) =>
          blueprints.flatMap(_.getNextStates)
        }
        .maxBy(_.resources.geode)
    )

  private def qualityLevelSum: Int =
    getMaxGeodes(24, blueprints).map(_.qualityLevel).sum

  private def geodeMult: Int =
    getMaxGeodes(32, blueprints.take(3)).map(_.resources.geode).product

  override def part1: Any = qualityLevelSum
  override def part2: Any = geodeMult

  @main def main(): Unit = run()
