pr = print

foo = function()
    print("nice")
end

print = function() end

foo()

pr("well!")
