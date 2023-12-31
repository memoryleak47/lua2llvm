local m = {}

function m.new(x, y)
    return { x, y }
end

function m.centroid(points, nrep)
    local x = 0.0
    local y = 0.0
    local npoints = #points
    for _ = 1, nrep do
        x = 0.0
        y = 0.0
        for i = 1, npoints do
            local p = points[i]
            x = x + p[1]
            y = y + p[2]
        end
    end
    return { x / npoints, y / npoints }
end


local N     = 10000
local nrep  = 50000

local arr = {}
for i = 1, N do
    local d = i * 3.1415
    arr[i] = m.new(d, d)
end

local r = m.centroid(arr, nrep)
print(r[1], r[2])
