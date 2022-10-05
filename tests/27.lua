printall = function(x, ...)
    if x then
        print(x)
        print(#{...})
        printall(...)
    end
end
printall(1, 2, 3)

