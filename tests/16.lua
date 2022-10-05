x = {name = "john"}
foo = function()
    local x = x.name
    return "hello " .. x
end
print(foo())

print("ok" and "nice")
print(not nil)

if "xy" == "x" .. "y" then
    print("yup!")
end
