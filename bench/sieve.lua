local m = {}

m.sieve = function(N)
    local is_prime = {}
    is_prime[1] = false
    for n = 2, N do
        is_prime[n] = true
    end

    local nprimes = 0
    local primes = {}

    for n = 1, N do
        if is_prime[n] then
            nprimes = nprimes + 1;
            primes[nprimes] = n
            for m = n+n, N, n do
                is_prime[m] = false
            end
        end
    end

    return primes
end

local N     = 100000
local nrep  = 1000

local ps
for _ = 1, nrep do
    ps = m.sieve(N)
end
print(#ps)
