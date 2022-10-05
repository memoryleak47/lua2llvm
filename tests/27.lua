printall = function(x, ...)
    if x then
        print(x)
        printall(...)
    end
end
printall(1, 2, 3)
