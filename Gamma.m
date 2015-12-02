(* ::Package:: *)

(*
	Copyright 2015 Lukas Schneiderbauer (lukas.schneiderbauer@gmail.com)


    This file is part of BProbe.

    BProbe is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    BProbe is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BProbe.  If not, see <http://www.gnu.org/licenses/>.

*)



BeginPackage["BProbe`Gamma`"];

	MatrixRepGamma::usage="MatrixRepGamma[d] returns a list of matrices which constitute a representation of the Clifford algebra Cl[R^d].";

Begin["`Private`"];

MatrixRepGamma[n_] := Block[{gamma, gamma5}, (* Gamma matrices of Cl_n( R ) *)
	
	If[n < 2,
		Print["Gamma matrices for n<2 not possible."];
		Abort[];
	];
	
	(* for n = 2m: recursion *)
	If[Mod[n,2]==0,
		If[n == 2,
			
			Return[PauliMatrix[{1,2}]];
			
		,
		
			gamma = MatrixRepGamma[n-2];
			gamma5 = (-I)^((n-2)/2) (Dot @@ gamma);
			
			Return[
				Join[
					KroneckerProduct[#, IdentityMatrix[2]]& /@ gamma,
					KroneckerProduct[gamma5, #]& /@ PauliMatrix[{1,2}]
				]
			];
		
		];
	,
	
		gamma = MatrixRepGamma[n-1];
		gamma5 = (-I)^((n-1)/2) (Dot @@ gamma);
		
		Return[Append[gamma,gamma5]];
	
	];
	
];

End[];
EndPackage[];
