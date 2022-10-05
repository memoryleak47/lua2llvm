x = {
    foo = function() return {foo = print} end,
    bar = {foo = print}
}

x:foo()
x.bar:foo();
(x:foo()):foo()
