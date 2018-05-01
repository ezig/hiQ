% Map each abstract 
abs(0..n_abs).
{ abstophys(X,0..n_phys) } = 1 :- abs(X).

% Qubit assignment rules

% Cannot assign different abstract qubits to the same physical qubit
:- X != Y, abstophys(X, P), abstophys(Y, P).
% Must have physical 2 qubit gates for abstract gates
:- abstophys(A1, P1), abstophys(A2, P2), absgate(A1, A2, G), not physgate(P1, P2, _, G).

% Set priority of readout error higher
#minimize { F@1, A, P, G : abstophys(A, P), absgate(A, _, G), physgate(P, _, F, G) }.
%#minimize { F@2, A, P : abstophys(A, P), absgate(A, _, "measure"), physgate(P, _, F, "measure") }.

#show abstophys/2.
