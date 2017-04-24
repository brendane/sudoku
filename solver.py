#!/usr/bin/env python2.7

from collections import Counter, defaultdict
import sys

def getrow(rc, board):
    r, c = rc
    return [board[(r, i)] for i in range(0, 9) if i != c]

def getcol(rc, board):
    r, c = rc
    return [board[(i, c)] for i in range(0, 9) if i != r]

def getsqr(rc, board):
    r, c = rc
    sc = c / 3
    sr = r / 3
    ret = []
    for i in range(3):
        for j in range(3):
            if (i+sr*3, j+sc*3) == rc:
                continue
            ret.append(board[(i+sr*3, j+sc*3)])
    return ret

def printboard(board):
    sys.stdout.write('\n||=====================================||\n')
    for i in range(9):
        sys.stdout.write('||')
        for j in range(9):
            c = board[(i, j)]
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

def printboard_full(board):
    for i in range(9):
        for j in range(9):
            if j > 0:
                sys.stdout.write('\t')
            sys.stdout.write(','.join(str(x) for x in board[(i, j)]))
            if len(board[(i, j)]) == 0:
                sys.stdout.write('??')
        sys.stdout.write('\n')

_board = {}

for infile in sys.argv[1:]:
    with open(infile, 'rb') as ihandle:
        for i, line in enumerate(ihandle):
            row = line.split('\t')
            for j, col in enumerate(row):
                col = col.strip()
                if col == '':
                    _board[(i, j)] = None
                else:
                    _board[(i, j)] = int(col)

    board = {}
    for rc, v in _board.iteritems():
        if v is None:
            board[rc] = set(range(1, 10))
        else:
            board[rc] = set([_board[rc]])

    print '\nStart: %s' % infile
    printboard(board)

    iter = 0
    progress = True
    while(progress):
        iter += 1
        progress = False
        for rc, v in board.iteritems():
            c = board[rc]
            if len(c) == 1:
                continue
            l = len(c)
            fixed = set()
            ufixed_row = set()
            for cc in getrow(rc, board):
                if len(cc) == 1:
                    fixed.update(cc)
                else:
                    ufixed_row.update(cc)
            ufixed_col = set()
            for cc in getcol(rc, board):
                if len(cc) == 1:
                    fixed.update(cc)
                else:
                    ufixed_col.update(cc)
            ufixed_sqr = set()
            for cc in getsqr(rc, board):
                if len(cc) == 1:
                    fixed.update(cc)
                else:
                    ufixed_sqr.update(cc)
            if len(c - fixed) == 0:
                c -= fixed
                printboard_full(board)
                print iter
                raise Exception('All numbers eliminated at %i, %i' % rc)
            c -= fixed
            if len(c - ufixed_row) == 1:
                c -= ufixed_row
            elif len(c - ufixed_col) == 1:
                c -= ufixed_col
            elif len(c - ufixed_sqr) == 1:
                c -= ufixed_sqr
            if len(c) < l:
                progress = True
            if len(c) == 1:
                continue

            for getfun in [getrow, getcol, getsqr]:
                single_overlaps = Counter()
                group_overlaps = defaultdict(int)
                cells = getfun(rc, board)
                for cc in cells:
                    if len(cc) == 1:
                        continue
                    single_overlaps.update(c.intersection(cc))
                    group_overlaps[tuple(c.intersection(cc))] += 1
                pairs = []
                triples = []
                quads = []
                quints = []
                for o in group_overlaps:
                    if len(o) == 2 and group_overlaps[o] == 1:
                        if single_overlaps[o[0]] == 1 and single_overlaps[o[1]] == 1:
                            pairs.append(o)
                    if len(o) == 3 and group_overlaps[o] == 2:
                        if single_overlaps[o[0]] == 2 and single_overlaps[o[1]] == 2 and \
                            single_overlaps[o[2]] == 2:
                            triples.append(o)
                    if len(o) == 4 and group_overlaps[o] == 3:
                        if single_overlaps[o[0]] == 3 and single_overlaps[o[1]] == 3 and \
                            single_overlaps[o[2]] == 3 and single_overlaps[o[3]] == 3:
                            quads.append(o)
                    if len(o) == 5 and group_overlaps[o] == 4:
                        if single_overlaps[o[0]] == 4 and single_overlaps[o[1]] == 4 and \
                            single_overlaps[o[2]] == 4 and single_overlaps[o[3]] == 4 and \
                            single_overlaps[o[4]] == 4:
                            quints.append(o)
                if len(quints) == 1:
                    c.clear()
                    c.update(quints[0])
                if len(quads) == 1:
                    c.clear()
                    c.update(quads[0])
                if len(triples) == 1:
                    c.clear()
                    c.update(triples[0])
                if len(pairs) == 1:
                    c.clear()
                    c.update(pairs[0])
                if len(c) < l:
                    progress = True
                if len(c) == 1:
                    continue

                for x in range(len(cells)):
                    for y in range(len(cells)):
                        if x == y:
                            continue
                        if len(cells[x]) == 2 and len(cells[y]) == 2:
                            if tuple(cells[x]) == tuple(cells[y]):
                                c -= cells[x]
                if len(c) < l:
                    progress = True
                if len(c) == 1:
                    continue

                for x in range(len(cells)):
                    for y in range(len(cells)):
                        if x == y:
                            continue
                        for z in range(len(cells)):
                            if x == z or y == z:
                                continue
                            if len(cells[x]) == 3 and len(cells[y]) == 3 and \
                                    len(cells[z]) == 3:
                                if tuple(cells[x]) == tuple(cells[y]) and \
                                        tuple(cells[x]) == tuple(cells[z]):
                                    c -= cells[x]
                if len(c) < l:
                    progress = True
                if len(c) == 1:
                    continue

                ## Could add four cell combinations here, if needed

    print 'End:'
    printboard(board)

    if max(len(v) for v in board.itervalues()) > 1:
        printboard_full(board)
        print
        print '%i iterations before giving up' % iter
    else:
        print
        print '%i iterations to solve' % iter
