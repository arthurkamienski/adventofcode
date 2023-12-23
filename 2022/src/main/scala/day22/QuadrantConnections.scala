package day22

object QuadrantConnections:
  val testQuadrantConnections
      : Map[(Int, Int), Map[Direction, ((Int, Int), Direction)]] = Map(
    (0, 2) -> Map(
      Direction.Up -> ((1, 0),
      Direction.Down),
      Direction.Down -> ((1, 2),
      Direction.Down),
      Direction.Left -> ((1, 1),
      Direction.Down),
      Direction.Right -> ((2, 3),
      Direction.Left)
    ),
    (1, 0) -> Map(
      Direction.Up -> ((0, 2),
      Direction.Down),
      Direction.Down -> ((2, 2),
      Direction.Up),
      Direction.Left -> ((2, 3),
      Direction.Up),
      Direction.Right -> ((1, 1),
      Direction.Right)
    ),
    (1, 1) -> Map(
      Direction.Up -> ((0, 2),
      Direction.Right),
      Direction.Down -> ((2, 2),
      Direction.Right),
      Direction.Left -> ((1, 0),
      Direction.Left),
      Direction.Right -> ((1, 2),
      Direction.Right)
    ),
    (1, 2) -> Map(
      Direction.Up -> ((0, 2),
      Direction.Up),
      Direction.Down -> ((2, 2),
      Direction.Down),
      Direction.Left -> ((1, 1),
      Direction.Left),
      Direction.Right -> ((2, 3),
      Direction.Down)
    ),
    (2, 2) -> Map(
      Direction.Up -> ((1, 2),
      Direction.Up),
      Direction.Down -> ((1, 0),
      Direction.Up),
      Direction.Left -> ((1, 1),
      Direction.Up),
      Direction.Right -> ((2, 3),
      Direction.Right)
    ),
    (2, 3) -> Map(
      Direction.Up -> ((1, 2),
      Direction.Left),
      Direction.Down -> ((1, 0),
      Direction.Right),
      Direction.Left -> ((2, 2),
      Direction.Left),
      Direction.Right -> ((0, 2),
      Direction.Left)
    )
  )

  val quadrantConnections
      : Map[(Int, Int), Map[Direction, ((Int, Int), Direction)]] = Map(
    (0, 1) -> Map(
      Direction.Up -> ((3, 0),
      Direction.Right),
      Direction.Down -> ((1, 1),
      Direction.Down),
      Direction.Left -> ((2, 0),
      Direction.Right),
      Direction.Right -> ((0, 2),
      Direction.Right)
    ),
    (0, 2) -> Map(
      Direction.Up -> ((3, 0),
      Direction.Up),
      Direction.Down -> ((1, 1),
      Direction.Left),
      Direction.Left -> ((0, 1),
      Direction.Left),
      Direction.Right -> ((2, 1),
      Direction.Left)
    ),
    (1, 1) -> Map(
      Direction.Up -> ((0, 1),
      Direction.Up),
      Direction.Down -> ((2, 1),
      Direction.Down),
      Direction.Left -> ((2, 0),
      Direction.Down),
      Direction.Right -> ((0, 2),
      Direction.Up)
    ),
    (3, 0) -> Map(
      Direction.Up -> ((2, 0),
      Direction.Up),
      Direction.Down -> ((0, 2),
      Direction.Down),
      Direction.Left -> ((0, 1),
      Direction.Down),
      Direction.Right -> ((2, 1),
      Direction.Up)
    ),
    (2, 0) -> Map(
      Direction.Up -> ((1, 1),
      Direction.Right),
      Direction.Down -> ((3, 0),
      Direction.Down),
      Direction.Left -> ((0, 1),
      Direction.Right),
      Direction.Right -> ((2, 1),
      Direction.Right)
    ),
    (2, 1) -> Map(
      Direction.Up -> ((1, 1),
      Direction.Up),
      Direction.Down -> ((3, 0),
      Direction.Left),
      Direction.Left -> ((2, 0),
      Direction.Left),
      Direction.Right -> ((0, 2),
      Direction.Left)
    )
  )
