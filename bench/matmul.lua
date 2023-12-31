local m = {}

function m.matmul(A, B)
    local C = {}
    local NI = #A
    local NK = #B
    local NJ = #B[1]
    for i = 1, NI do
        local line = {}
        for j = 1, NJ do
            line[j] = 0.0
        end
        C[i] = line
    end
    for k = 1, NK do
        local Bk = B[k]
        for i = 1, NI do
            local Aik = A[i][k]
            local Ci = C[i]
            for j = 1, NJ do
                Ci[j] = Ci[j] + Aik * Bk[j]
            end
        end
    end
    return C
end

local N   = 800
local REP = math.max(1.0, 2 * (800/N)^3)

-- Suggested values for N, REP:
--  800,    2
--  400,   16
--  200,  128
--  100, 1024

local A = {}
for i = 1, N do
    A[i] = {}
    for j = 1, N do
        A[i][j] = (i + j) * 3.1415
    end
end

local C
for _ = 1, REP do
    C = m.matmul(A, A)
end
print("#C", #C, #C[1])
print("C[1][1]", C[1][1])
