obj = {x = function(this, other)
    print(this.y)
    print(other) end
, y=2}

obj:x(3)

