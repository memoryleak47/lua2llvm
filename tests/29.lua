mkref = function()
    local x = 2
    local setter = function(y) x=y end
    local getter = function() return x end
    return setter, getter
end

s, g = mkref()
print(g())
s(20)
print(g())

s2, g2 = mkref()
print(g2())
print(g())
