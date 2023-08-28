x = function() end
print(x == x) -- true

x = function() return function() end end
print(x() == x()) -- false
