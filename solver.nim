import os, parsecsv, sets, strutils

type
  Cell = HashSet[int]
  Board = seq[Cell]
  CellState = enum
    Solved
    Improved
    Unsolved
    Wrong

proc emptyCell(): Cell =
  return toSet([1, 2, 3, 4, 5, 6, 7, 8, 9])

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
  for j, elem in b:
    if i == j:
      continue
    if j /% 9 == r or j %% 9 == c or ((j%%9)/%3 == sc and j /% 27 == sr):
      yield b[j]

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
## improvement
proc simpleSolve(b:var Board): bool =
  var improvement:bool = true
  var solved:int = 0
  while improvement or solved < 81:
    improvement = false
    solved = 0
    for i, c in b:
      if len(b[i]) == 1:
        solved += 1
        continue
      var new_cell = simpleElim(b, i)
      if len(c) > len(new_cell):
        improvement = true
      if len(new_cell) == 0:
        raise newException(Exception, "all possibilities used")
      b[i] = new_cell
  return solved == 81

## Default is for arguments to be immutable
proc guessSolve(b:Board): bool =
  return true

proc main(): void =
  var board = readBoard("1.txt")
  printBoard(board)
  var solved:bool = simpleSolve(board)
  echo "Solved? ", $solved
  printBoard(board)
main()
