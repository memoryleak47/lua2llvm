local x = 20

do
    local x = 1
    print(x)
end
print(x)

if true then
    local x = 1
    print(x)
end
print(x)

if false then
else
    local x = 1
    print(x)
end
print(x)
