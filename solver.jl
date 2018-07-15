#!/usr/bin/env julia

function print_board(b)
    for i in 1:9
        if (i-1) % 3 == 0
            print("||", "="^37, "||\n")
        end
        for j in 1:9
            if (j-1) % 3 == 0
                print("|")
            end
            if length(b[i, j]) == 1
                print("| ", collect(b[i, j])[1], " ")
            else
                print("| ? ")
            end
        end
        print("||\n")
    end
    print("||", "="^37, "||\n\n")
end

function n_unsolved(b)
    n_unsolved = 0
    for cell in b
        if length(cell) != 1
            n_unsolved += 1
        end
    end
    n_unsolved
end

function read_board(fname)
    board = Array{Set{Int},2}(undef, 9, 9)
    open(fname, "r") do handle
        for (i, line) in enumerate(eachline(handle))
            for (j, cell) in enumerate(split(line, "\t"))
                if cell == ""
                    board[i, j] = Set{Int}(1:9)
                else
                    board[i, j] = Set{Int}(parse(Int, cell))
                end
            end
        end
    end
    board
end

function get_rc(b, r, c, row=true)
    ret = Array{Set{Int},1}()
    for i in 1:9
        if (row && i != c) || (!row && i != r)
            if row
                push!(ret, b[r, i])
            else
                push!(ret, b[i, c])
            end
        end
    end
    ret
end

function get_sqr(b, r, c)
    ret = Array{Set{Int},1}()
    sqr_row = div((r-1), 3) * 3 + 1
    sqr_col = div((c-1), 3) * 3 + 1
    for i in sqr_row:(sqr_row + 2)
        for j in sqr_col:(sqr_col+2)
            if !(i == r && j == c)
                push!(ret, b[i, j])
            end
        end
    end
    ret
end


function get_neighbors(b, r, c)
    [get_rc(b, r, c, true), get_rc(b, r, c, false), get_sqr(b, r, c)]
end

function simple_solver!(b)
    n_u = n_unsolved(b)
    while n_u > 0
        for (i, j) in Base.Iterators.product(1:9, 1:9)
            cell = b[i, j]
            if length(cell) == 1
                continue
            end
            neighbors = get_neighbors(b, i, j)
            for n in neighbors
                for c in n
                    if length(c) == 1
                        cc = setdiff(cell, c)
                        if length(cc) < 1
                            throw("too few at $i, $j")
                        end
                        b[i, j] = cc
                        cell = cc
                    end
                end
            end
            if length(cell) == 1 continue end
            r = setdiff(cell, reduce(union, neighbors[1]))
            if length(r) == 1
                b[i, j] = Set{Int}(r)
                continue
            end
            c = setdiff(cell, reduce(union, neighbors[2]))
            if length(c) == 1
                b[i, j] = Set{Int}(c)
                continue
            end
            s = setdiff(cell, reduce(union, neighbors[3]))
            if length(s) == 1
                b[i, j] = Set{Int}(s)
                continue
            end
        end
        n_u_ = n_unsolved(b)
        if n_u_ == n_u
            # No progress
            break
        end
        n_u = n_u_
    end
end

function solve(b, guess_cells=[])
    guess_cell = (0, 0)
    for n_to_guess in 2:9
        for (i, j) in Base.Iterators.product(1:9, 1:9)
            if length(b[i, j]) != n_to_guess || (i, j) in guess_cells
                continue
            else
                guess_cell = (i, j)
                break
            end
        end
    end
    b_cpy = deepcopy(b)
    for c in b[guess_cell[1], guess_cell[2]]
        b_cpy = deepcopy(b)
        b_cpy[guess_cell[1], guess_cell[2]] = Set{Int}(c)
        try
            simple_solver!(b_cpy)
        catch
            continue
        end
        if n_unsolved(b_cpy) == 0
            return b_cpy
        else
            solution = solve(b_cpy, push!(copy(guess_cells), guess_cell))
            if n_unsolved(solution) == 0
                return solution
            else
                continue
            end
        end
    end
    ## If a guess at a higher level was wrong
    return b_cpy
end

function main()
    for fname in ARGS
        board = read_board(fname)
        print("\n", "Start: ", fname, " (", n_unsolved(board), ")\n\n")
        print_board(board)
        solution = solve(board)
        print("\n", "End: ", fname, " (", n_unsolved(solution), ")\n\n")
        print_board(solution)
    end
end

main()
