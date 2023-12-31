local m = {}

local ALIVE = "*"
local DEAD  = " "

-- Create a new grid for the simulation.
m.new_canvas = function(N, M)
    local t = {}
    for i = 1, N do
        local line = {}
        for j = 1, M do
            line[j] = 0
        end
        t[i] = line
    end
    return t
end

-- Our grid has a toroidal topology with wraparound
m.wrap = function(i, N)
    return (i - 1) % N + 1
end

-- Print the grid to stdout.
m.draw = function(N, M, cells)
    local out = "" -- accumulate to reduce flicker
    for i = 1, N do
        local cellsi = cells[i]
        out = out .. "|"
        for j = 1, M do
            if cellsi[j] ~= 0 then
                out = out .. ALIVE
            else
                out = out .. DEAD
            end
        end
        out = out .. "|\n"
    end
    io.write(out)
end

-- Place a shape in the grid
m.spawn = function(N, M, cells, shape, top, left)
    for i = 1, #shape do
        local ci = m.wrap(i+top-1, N)
        local shape_row = shape[i]
        local cell_row = cells[ci]
        for j = 1, #shape_row do
            local cj = m.wrap(j+left-1, M)
            cell_row[cj] = shape_row[j]
        end
    end
end

-- Run one step of the simulation.
m.step = function(N, M, curr_cells, next_cells)
    for i2 = 1, N do
        local i1 = m.wrap(i2-1, N)
        local i3 = m.wrap(i2+1, N)

        local cells1 = curr_cells[i1]
        local cells2 = curr_cells[i2]
        local cells3 = curr_cells[i3]

        local next2 = next_cells[i2]

        for j2 = 1, M do
            local j1 = m.wrap(j2-1, M)
            local j3 = m.wrap(j2+1, M)

            local c11 = cells1[j1]
            local c12 = cells1[j2]
            local c13 = cells1[j3]

            local c21 = cells2[j1]
            local c22 = cells2[j2]
            local c23 = cells2[j3]

            local c31 = cells3[j1]
            local c32 = cells3[j2]
            local c33 = cells3[j3]

            local sum = c11 + c12 + c13 + c21 + c23 + c31 + c32 + c33

            if sum == 3 or (sum == 2 and (c22 == 1)) then
                next2[j2] = 1
            else
                next2[j2] = 0
            end
        end
    end
end

local nsteps = 2000

local GLIDER = {
    { 0,0,1, },
    { 1,0,1, },
    { 0,1,1, }
}

local N = 40
local M = 80

local curr_cells = m.new_canvas(N, M)
local next_cells = m.new_canvas(N, M)

for i = 1, 8 do
    for j = 1, 16 do
        local i0 = 5*i + 1 + j*j
        local j0 = 5*j + 1
        m.spawn(N, M, curr_cells, GLIDER, i0, j0)
    end
end

io.write("\027[2J")	-- TODO support ANSI clear screen
for _ = 1, nsteps do
    m.step(N, M, curr_cells, next_cells)
    curr_cells, next_cells = next_cells, curr_cells
    io.write("\027[H") -- TODO support ANSI home cursor
    m.draw(N, M, curr_cells)
end
