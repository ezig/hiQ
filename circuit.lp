% Number of abstract and physical qubits
#const n_abs = 3.
#const n_phys = 3.

% Map each abstract 
abs(1..n_abs).
{ abstophys(X,1..n_phys) } = 1 :- abs(X).

% Qubit assignment rules

% Cannot assign different abstract qubits to the same physical qubit
:- X != Y, abstophys(X, P), abstophys(Y, P).
% Must have physical 2 qubit gates for abstract gates
:- abstophys(A1, P1), abstophys(A2, P2), absgate(A1, A2, G), not physgate(P1, P2, G).

#show abstophys/2.

% Physical gate constraints
physgate(2, 3, "CNOT").
physgate(2, 1, "CNOT").
