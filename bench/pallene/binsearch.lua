local m = {}

m.binsearch = function(t, x)
    -- lo <= x <= hi
    local lo = 1
    local hi = #t

    local steps = 0

    while lo < hi do

        local mid = lo + (hi - lo) / 2 -- TODO this should be a // division
        steps = steps + 1

        local tmid = t[mid]

        if x == tmid then
            return steps
        elseif x < tmid then
            hi = mid - 1
        else
            lo = mid + 1
        end
    end

    return steps
end

m.test = function(t, nrep)
    local s = 0
    for i = 1, nrep do
        if m.binsearch(t, i) ~= 22 then
            s = s + 1
        end
    end
    return s
end

local N    = 1000000
local nrep = N

local t = {}
for x = 1, N do
    t[x] = x
end

local r = m.test(t, nrep)
print(r)
