%------------------------------------------------------------------------------
% File     : SYN986+1.004 : TPTP v4.0.0. Released v4.0.0.
% Domain   : Syntactic
% Problem  : Orevkov formula - size 4
% Version  : [TS00] axioms : Especial.
% English  : hyp(2,k,1) is defined. Each Ck has a non-normal natural deduction
%            of size linear in k, and each normal deduction of Ck has at least
%            hyp(2,k,1)=2_k nodes.

% Refs     : [TS00]  Troelska & Schwichtenberg (2000), Basic Proof Theory
%          : [Rat08] Raths (2008), Email to G. Sutcliffe
% Source   : [Rat08]
% Names    :

% Status   : Theorem
% Rating   : 0.74 v4.0.0
% Syntax   : Number of formulae    :    3 (   1 unit)
%            Number of atoms       :    9 (   0 equality)
%            Maximal formula depth :   10 (   6 average)
%            Number of connectives :    6 (   0 ~  ;   0  |;   4  &)
%                                         (   0 <=>;   2 =>;   0 <=)
%                                         (   0 <~>;   0 ~|;   0 ~&)
%            Number of predicates  :    1 (   0 propositional; 3-3 arity)
%            Number of functors    :    2 (   1 constant; 0-1 arity)
%            Number of variables   :   10 (   0 singleton;   5 !;   5 ?)
%            Maximal term depth    :    2 (   1 average)

% Comments : Inspiration by Wolfgang Bibel, Helmut Schwichtenberg.
%          : The complexity of proof seach is hyper-exponential with the size
%            of the formulae: 2^0=1, 2^1=2, 2^2=4, 2^4=16, 2^16=65536,
%            2^65536 = mmmh... ?
%------------------------------------------------------------------------------
%----Include Orevkov formula
include('Axioms/SYN002+0.ax').
%------------------------------------------------------------------------------
fof(ck,conjecture,(
    ? [Z4,Z3,Z2,Z1,Z0] :
      ( r(zero,zero,Z4)
      & r(zero,Z4,Z3)
      & r(zero,Z3,Z2)
      & r(zero,Z2,Z1)
      & r(zero,Z1,Z0) ) )).

%------------------------------------------------------------------------------
