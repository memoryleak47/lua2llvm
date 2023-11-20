f = function(i, n)
    if i==n then return i
    end
    return i, f(i+1, n)
end


print(#{f(0, 10)})

