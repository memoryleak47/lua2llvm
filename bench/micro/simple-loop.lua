-- This is quite a bit slower than the Lua interpreter.
-- It might be due to the fact that we generate unnecessary blocks & jumps.

local y = 0
local x = 0
while x <= 10000000 do
    y = (y*2 + x) % 21
    x = x+1
end
print(y)

