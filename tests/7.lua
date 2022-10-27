foo = function()    
    local x = 200
    local y = 2
    local x -- redeclare
    print(x)
end

foo()
