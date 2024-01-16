# O_1(n) 
# iterative algorithm
function O1_iter(n)
    S = 0
    for i = 1:n
        S = S + 2*i-1
    end
    return S
end

# recursive algorithm
function O1_rec(n)
    if n > 1
        return 2*n - 1 + O1_rec(n-1)
    else
        return 1
    end
end

# verify 
n_values = [1, 10, 100]
for n in n_values
    iter_result = O1_iter(n)
    rec_result = O1_rec(n)
    real_result = n^2
    println("n: $n, Iterative result: $iter_result, 
    Recursive result: $rec_result, Real result: $real_result")
end

# S_2(n) 
# iterative algorithm
function S2_iter(n)
    S = 0
    for i = 1:n
        S = S + i^2
    end
    return S
end

# recursive algorithm
function S2_rec(n)
    if n > 1
        return n^2 + S2_rec(n-1)
    else
        return 1
    end
end

# verify 
for n in n_values
    iter_result = S2_iter(n)
    rec_result = S2_rec(n)
    real_result = (n*(n+1)*(2*n+1)) ÷ 6
    println("n: $n, Iterative result: $iter_result, 
    Recursive result: $rec_result, Real result: $real_result")
end
