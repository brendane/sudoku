import os, parsecsv, sets, strutils

type
  Cell = HashSet[int]
  Board = seq[Cell]
  BoardState = enum
    Solved
    Unsolved
    Failed


proc emptyCell(): Cell =
  return toSet([1, 2, 3, 4, 5, 6, 7, 8, 9])


proc nSolved(b:Board): int =
  result = 0
  for c in b:
    if len(c) == 1:
      result += 1


proc readBoard(fname:string): Board =
  result = @[]
  var p: CsvParser
  p.open(fname, separator='\t')
  while p.readRow():
    for val in items(p.row):
      if val == "":
        result.add(emptyCell())
      else:
        result.add(toSet([parseInt(val)]))
  p.close()


proc printBoard(b:Board): void =
  echo "\n||=====================================||"
  for i, elem in b:
    if i %% 3 == 0:
      write stdout, "|"
    write stdout, "| "
    if elem.len == 1:
      for e in elem:
        write stdout, e, " "
    else:
      write stdout, "? "
    if (i+1) %% 9 == 0:
      write stdout, "||\n"
      if ((i+1) /% 9) %% 3 == 0:
        echo "||=====================================||"
  echo ""

iterator getNeighbors(b:Board; i:int): Cell =
  let r:int = i /% 9
  let c:int = i %% 9
  let sr:int = i /% 27
  let sc:int = (i %% 9) /% 3
  for j in (r*9)..(r*9+8):
    if i == j:
      continue
    yield b[j]
  for j in 0..8:
    if i == c + j * 9:
      continue
    yield b[c+j*9]
  for j in 0..2:
    for k in 0..2:
      var idx = sr * 27 + j * 9 + k + sc * 3
      if idx == i:
        continue
      yield b[idx]


## Modify this cell using simple elimination
## b is immutable at present -- probably more efficient to
## change that so the cell can be modified directly
proc simpleElim(b:var Board; i:int): Cell =
  var c = b[i]
  for n in getNeighbors(b, i):
    if len(n) == 1:
      c = c - n
  return c


## Run simpleElim on all cells, repeating until there is no more
## improvement; modifies b.
proc simpleSolve(b:var Board): BoardState =
  var improvement:bool = true
  var solved:int = 0
  while improvement and solved < 81:
    improvement = false
    solved = 0
    for i, c in b:
      if len(b[i]) == 1:
        solved += 1
        continue
      var new_cell = simpleElim(b, i)
      if len(new_cell) == 0:
        return Failed
      if len(c) > len(new_cell):
        improvement = true
      b[i] = new_cell
  if solved == 81:
    return Solved
  else:
    return Unsolved


iterator pickGuess(b:Board): int =
  for j in 2..9:
    for i, c in b:
      if len(c) == j:
        yield i


proc guessSolve(b:Board): (BoardState, Board) =
  var n: int = nSolved(b)
  if n == 81:
    return (Solved, b)
  var state: BoardState
  for i in pickGuess(b):
    for j in b[i]:
      var bb: Board
      deepCopy(bb, b)
      bb[i] = Cell(toSet([j]))
      state = simpleSolve(bb)
      if state == Solved:
        return (state, bb)
      elif state == Failed:
        continue
      else:
        (state, bb) = guessSolve(bb)
      if state == Solved:
        return (state, bb)
      else:
        continue
    return (Failed, b)


proc solve(fname:string): (string, Board, Board, BoardState) =
  var start: Board = readBoard(fname)
  var final: Board
  deepCopy(final, start)
  var state: BoardState = simpleSolve(final)
  if state == Unsolved:
    (state, final) = guessSolve(final)
  return (fname, start, final, state)


proc main(): void =
  for fname in commandLineParams():
    var board = readBoard(fname)
    echo fname
    printBoard(board)
    var state: BoardState = simpleSolve(board)
    if state == Unsolved:
      (state, board) = guessSolve(board)
    echo "Current state: ", $state
    printBoard(board)

main()
