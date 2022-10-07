x = 1
for ctr = 1, 5 do
    print(x) -- should always be the global x, never the local from the previous iteration.
    local x = 4
end
