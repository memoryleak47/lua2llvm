local x, y, z = 1, 2, 3
foo = function()
    return 1, 2
end
print(x)
print(y)
print(z)

x, y, z = foo()
print(x)
print(y)
print(z)

print(#{foo()})
print(#{foo(), nil})
