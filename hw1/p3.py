# O_1(n)
def O1_iter(n):
    S = 0
    for i in range(1, n+1):
        S = S + 2*i-1
    return S

def O1_rec(n):
    if n > 1:
        return 2*n-1 + O1_rec(n-1)
    else:
        return 1

# Verify
n_values = [1, 10, 100]
results = []

for n in n_values:
    iter_result = O1_iter(n)
    rec_result = O1_rec(n)
    real_result = n**2
    results.append((n, iter_result, rec_result, real_result))

for n, iter_result, rec_result, real_result in results:
    print(f"n: {n}, Iterative result: {iter_result}, "\
          f"Recursive result: {rec_result}, Real result: {real_result}")



# S_2(n)
def S2_iter(n):
    S = 0
    for i in range(1, n+1):
        S = S + i**2
    return S

def S2_rec(n):
    if n > 1:
        return n**2 + S2_rec(n-1)
    else:
        return 1

# Verify
n_values = [1, 10, 100]
results = []

for n in n_values:
    iter_result = S2_iter(n)
    rec_result = S2_rec(n)
    real_result = (n*(n+1) * (2*n+1)) // 6
    results.append((n, iter_result, rec_result, real_result))

for n, iter_result, rec_result, real_result in results:
    print(f"n: {n}, Iterative result: {iter_result}, "\
          f"Recursive result: {rec_result}, Real result: {real_result}")
