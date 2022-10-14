local fff = 12
foo = function()
    local f = function()
        return function()
           fff = fff-1;
        end;
    end;
    (f())();
    return f()
end;


(foo())();
print(fff)
