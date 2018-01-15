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



BeginPackage["BProbeM`SU2Gen`"];

(*
		"MatrixRepSU2[n] returns a list of matrices which are the irrep of " <>
		"the su(2) Lie-Algebra with dimension 'n'.\n\n" <>
		"Example: j = MatrixRepSU2[3]; j[[1]] gives the first matrix rep.";
*)


Begin["`Private`"];


MatrixRepSU2[dim_] := Block[{n=dim,i,t,matrix,com,Jmin,Jplu},
	(* generate list of equations *)
	(* qm convention: J\pm |jm> = sqrt( (j \mp m) (j \pm m+1) ) |j,m+1> *)
	(* m = i - n/2 - 1/2 *)
	(* j = (n-1)/2 *)

	matrix = Table[0,{n},{n}];

	For[i=1,i<n,i++, (* absteiger *)
		matrix[[i+1,i]] = Sqrt[(n-i)i];
	];
	
	Jmin = matrix;
	Jplu = ConjugateTranspose[Jmin];

	com[a_,b_]:=a.b - b.a;

	t = Table[{},{3}];
	t[[1]] = 1/2 (Jmin + Jplu);
	t[[2]] = 1/(2I) (Jplu - Jmin);
	t[[3]] = 1/I com[t[[1]],t[[2]]];

	Return[t];
];


End[];
EndPackage[];