(* ::Package:: *)

(*

	This file is part of BProbeM.
	
	"BProbeM, quantum and fuzzy geometry scanner" Copyright 2018 Timon Gutleb (timon.gutleb@gmail.com),
	see LINK
	
	Original version "BProbe" Copyright 2015 Lukas Schneiderbauer (lukas.schneiderbauer@gmail.com),
	see https://github.com/lschneiderbauer/BProbe

    BProbeM and BProbe are free software: you can redistribute them and/or modify
    them under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    BProbeM and BProbe are distributed in the hope that they will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BProbeM. If not, see <http://www.gnu.org/licenses/>.

*)



BeginPackage["BProbeM`Gamma`"];

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
