for x, y, z in pairs({x = "ok"}) do
    print(x)
    print(y)
    print(z)
end

local s = 0
for idx, y in pairs({5, 19}) do
    s = s + idx + y
end
print(s)
