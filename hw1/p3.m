% Main script for O1 calculations
disp('O1 Calculations:');
n1 = [1, 10, 100];
o1_iter_result = arrayfun(@(x) O1_iter(x), n1);
o1_rec_result = arrayfun(@(x) O1_rec(x), n1);
o1_real_result = n1.^2;

% Displaying O1 results
T1 = table(n1', o1_iter_result', o1_rec_result', o1_real_result', ...
           'VariableNames', {'n', 'IterativeResult', 'RecursiveResult', 'RealResult'});
disp(T1)

% Main script for S2 calculations
disp('S2 Calculations:');
n2 = [1, 10, 100];
s2_iter_result = arrayfun(@(x) S2_iter(x), n2);
s2_rec_result = arrayfun(@(x) S2_rec(x), n2);
s2_real_result = (n2.*(n2+1).*(2.*n2+1))/6;

% Displaying S2 results
T2 = table(n2', s2_iter_result', s2_rec_result', s2_real_result', ...
           'VariableNames', {'n', 'IterativeResult', 'RecursiveResult', 'RealResult'});
disp(T2)

% Local functions
function S = O1_iter(n)
    S = 0;
    for i = 1:n
        S = S + 2*i - 1;
    end
end

function S = O1_rec(n)
    if n > 1
        S = 2*n - 1 + O1_rec(n - 1);
    else
        S = 1;
    end
end

function S = S2_iter(n)
    S = 0;
    for i = 1:n
        S = S + i^2;
    end
end

function S = S2_rec(n)
    if n > 1
        S = n^2 + S2_rec(n - 1);
    else
        S = 1;
    end
end
