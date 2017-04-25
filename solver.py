#!/usr/bin/env python2.7

from collections import Counter, defaultdict
from copy import deepcopy
from itertools import combinations as combn
import sys

def row_cells(rc, sudoku):
    """Given coordinates rc, return all the other cells in the same row"""
    r, c = rc
    return [sudoku[(r, i)] for i in range(0, 9) if i != c]

def col_cells(rc, sudoku):
    """Given coordinates rc, return all the other cells in the same column"""
    r, c = rc
    return [sudoku[(i, c)] for i in range(0, 9) if i != r]

def sqr_cells(rc, sudoku):
    """Given coordinates rc, return all the other cells in the same square"""
    r, c = rc
    sc = c / 3
    sr = r / 3
    ret = []
    for i in range(3):
        for j in range(3):
            if (i+sr*3, j+sc*3) == rc:
                continue
            ret.append(sudoku[(i+sr*3, j+sc*3)])
    return ret

def printsudoku(sudoku):
    sys.stdout.write('\n||=====================================||\n')
    for i in range(9):
        sys.stdout.write('||')
        for j in range(9):
            c = sudoku[(i, j)]
            if len(c) > 1:
                p = '?'
            else:
                p = str(list(c)[0])
            sys.stdout.write(' ' + p + ' |')
            if j % 3 == 2:
                sys.stdout.write('|')
        sys.stdout.write('\n')
        if i % 3 == 2:
            sys.stdout.write('||=====================================||\n')
    sys.stdout.write('\n')

def printsudoku_full(sudoku):
    for i in range(9):
        for j in range(9):
            if j > 0:
                sys.stdout.write('\t')
            sys.stdout.write(','.join(str(x) for x in sudoku[(i, j)]))
            if len(sudoku[(i, j)]) == 0:
                sys.stdout.write('??')
        sys.stdout.write('\n')

def n_unsolved(sudoku):
    """Get the number of cells left unsolved"""
    return sum(int(len(c) != 1) for c in sudoku.itervalues())

def read_sudoku_file(fname):
    b = {}
    with open(infile, 'rb') as ihandle:
        for i, line in enumerate(ihandle):
            row = line.split('\t')
            for j, col in enumerate(row):
                col = col.strip()
                if col == '':
                    b[(i, j)] = set(range(1, 10))
                else:
                    b[(i, j)] = set([int(col)])
    return b

def simple_elimination(cell, groups):
    """Use simple elimination methods to solve a cell. Modifies
    `cell'. """
    fixed = set()
    ufixed = []
    for group in groups:
        uf = set()
        for cc in group:
            if len(cc) == 1:
                fixed.update(cc)
            else:
                uf.update(cc)
        ufixed.append(uf)
    if len(cell - fixed) == 0:
        raise Exception('All numbers eliminated!')
    # Eliminate the numbers that this cell cannot be b/c
    # other cells are already set to those numbers
    cell -= fixed
    # Figure out if this is the only cell in a particular row,
    # column, or square that can be a certain number
    for uf in ufixed:
        if len(cell - uf) == 1:
            cell -= uf

def eliminate_with_matching_tuples(cell, groups, max_size=5):
    """Figure out if this cell is part of a pair (or triple, quadruple,
    etc.) that are the only cells containing a certain set of 2 (or 3, 4, etc.)
    items in the row, column, or square. If so, constrain the possibilities
    for this cell to just that set of items.

    Example: this cell is (1,3,4,7) and only one other cell in the row
    contains 3 and 7. Then make this cell just (3,7)."""
    for cells in groups:
        single_overlaps = Counter()
        group_overlaps = defaultdict(int)
        for cc in cells:
            if len(cc) == 1:
                continue
            single_overlaps.update(cell.intersection(cc))
            group_overlaps[tuple(cell.intersection(cc))] += 1
        for tuple_len in range(max_size, 1 , -1):
            tupls = []
            for o, ocount in group_overlaps.iteritems():
                if len(o) == tuple_len and ocount == tuple_len - 1:
                    match = True
                    for tuple_member in o:
                        if single_overlaps[tuple_member] != tuple_len - 1:
                            match = False
                    if match:
                        tupls.append(o)
            if len(tupls) == 1:
                cell.clear()
                cell.update(tupls[0])

def eliminate_with_other_tuples(cell, groups_with_cell, max_size=5):
    """Figure out if there are two other cells in this row,
    column or square that can each only be the same two
    items (e.g. two cells that are both (1,5) ).
    If so, remove those numbers from this cell's list.
    Extended to groups larger than two."""
    for cells in groups_with_cell:
        for size in range(max_size, 1, -1):
            for cell_comb in combn(filter(lambda x: len(x) == size, cells), size):
                tupls = map(tuple, cell_comb)
                match = reduce(lambda x, y: x == y, tupls)
                if match:
                    cell -= cell_comb[0]
            if len(cell) == 1:
                return

def logical_solver(sudoku):
    """Solve the sudoku using logical methods"""
    iteration = 0
    progress = True
    while(progress):
        iteration += 1
        progress = False
        for rc, c in sudoku.iteritems():
            if len(c) == 1:
                continue
            l = len(c)
            groups_with_cell = [row_cells(rc, sudoku), col_cells(rc, sudoku),
                sqr_cells(rc, sudoku)]

            simple_elimination(c, groups_with_cell)
            if len(c) < l:
                progress = True
            if len(c) == 1:
                continue

            eliminate_with_matching_tuples(c, groups_with_cell)
            if len(c) < l:
                progress = True
            if len(c) == 1:
                continue
                
            eliminate_with_other_tuples(c, groups_with_cell)
            if len(c) < l:
                progress = True
            if len(c) == 1:
                continue

    return sudoku, iteration, n_unsolved(sudoku) == 0

def guess_solver(sudoku, prev_chosen=[]):
    """Take a guess at one undetermined cell, then try to solve"""
    cell_to_guess = None
    for n_choices in range(2, 10):
        if cell_to_guess is not None:
            break
        for rc, cell in sudoku.iteritems():
            if len(cell) == n_choices:
                if rc in prev_chosen:
                    continue
                cell_to_guess = rc
                break
    choices = list(sudoku[cell_to_guess])
    for choice in choices:
        sudoku_copy = deepcopy(sudoku)
        sudoku_copy[cell_to_guess] = set([choice])
        solved = False
        try:
            sudoku_copy, niter, solved = logical_solver(sudoku_copy)
        except:
            continue # Failed with this guess, try next choice
        # Probably not the most elegant way to code this, but it has
        # to be able to detect when no solution is found.
        if solved:
            return True, sudoku_copy
        else:
            solved, sudoku_copy = guess_solver(sudoku_copy, prev_chosen + [cell_to_guess])
        if solved:
            return True, sudoku_copy
        else:
            continue
    return False, sudoku_copy # if guessing fails - probably not all choices tried or logic wrong


for infile in sys.argv[1:]:
    start_sudoku = read_sudoku_file(infile)
    print '\nStart: %s (%i)' % (infile, n_unsolved(start_sudoku))
    printsudoku(start_sudoku)

    solved = False
    try:
        end_sudoku, niters, solved = logical_solver(deepcopy(start_sudoku))
    except:
        solved = False
        end_sudoku = start_sudoku

    if not solved:
        end_sudoku = guess_solver(end_sudoku)[1]
        print 'End: (%i)' % n_unsolved(end_sudoku)
        printsudoku(end_sudoku)
    else:
        print 'End: (%i)' % n_unsolved(end_sudoku)
        printsudoku(end_sudoku)
