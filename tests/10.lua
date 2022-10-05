wupsi = function() print("wupsidupsi") end
x = {
    foo = function() return {foo = wupsi} end,
    bar = {foo = wupsi}
}

x:foo()
x.bar:foo();
(x:foo()):foo()
