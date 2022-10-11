t = true
f = false

if t then print("a") else print("b") end; print("-")
if f then print("a") else print("b") end; print("-")

----

if t then print("a") elseif t then print("b") else print("c") end; print("-")
if t then print("a") elseif f then print("b") else print("c") end; print("-")
if f then print("a") elseif t then print("b") else print("c") end; print("-")
if f then print("a") elseif f then print("b") else print("c") end; print("-")

---- 

if t then print("a") elseif t then print("b") elseif t then print("c") else print("d") end; print("-")
if t then print("a") elseif t then print("b") elseif f then print("c") else print("d") end; print("-")
if t then print("a") elseif f then print("b") elseif t then print("c") else print("d") end; print("-")
if t then print("a") elseif f then print("b") elseif f then print("c") else print("d") end; print("-")

if f then print("a") elseif t then print("b") elseif t then print("c") else print("d") end; print("-")
if f then print("a") elseif t then print("b") elseif f then print("c") else print("d") end; print("-")
if f then print("a") elseif f then print("b") elseif t then print("c") else print("d") end; print("-")
if f then print("a") elseif f then print("b") elseif f then print("c") else print("d") end; print("-")
