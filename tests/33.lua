foo = function(txt, ret)
    print(txt)
    return ret
end

x = foo("a", true) or print("_")
x = foo("b", false) or print("_")
x = foo("c", true) and print("_")
x = foo("d", false) and print("_")
