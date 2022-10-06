mktwo = function()
    local x = 2
    local f = function() return x end
    x = 40
    return f
end

local two = mktwo()
print(two())
