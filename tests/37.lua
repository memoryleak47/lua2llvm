local x = 1;
(function()
    (function() x = 2 end)()
end)();
print(x)
