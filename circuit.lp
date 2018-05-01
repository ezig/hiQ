% Map each abstract 
abs(0..n_abs).
{ abstophys(X,0..n_phys) } = 1 :- abs(X).

% Qubit assignment rules

% Cannot assign different abstract qubits to the same physical qubit
:- X != Y, abstophys(X, P), abstophys(Y, P).
% Must have physical 2 qubit gates for abstract gates
:- abstophys(A1, P1), abstophys(A2, P2), absgate(A1, A2, G), not physgate(P1, P2, G).

#show abstophys/2.
