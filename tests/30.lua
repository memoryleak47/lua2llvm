foo = function()
    local x = 2
    return function()
        return function()
            x = 30
            return x
        end
    end
end

print(((foo())())())
