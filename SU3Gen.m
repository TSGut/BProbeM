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



BeginPackage["BProbe`SU3Gen`"];

(*
		"MatrixRepSU3[highestweight] returns a list of matrices which are the irrep of " <>
		"the su(3) Lie-Algebra with highest weight 'highestweight'.\n" <>
		"The parameter is to be expected in the form {n,m}.\n\n" <>
		"Example: t = MatrixRepSU3[{1,1}]; t[[1]] gives the first matrix rep.";
*)

Begin["`Private`"];


MatrixRepSU3[highestweight_] := Block[{irrep=highestweight,t,com},
	
	(* convert weight to GT-pattern format *)
	irrep = {irrep[[2]],irrep[[1]]}; (* swap *)
	irrep[[1]]+= irrep[[2]];
	irrep=Append[irrep,0];

	(* helper function *)
	com[a_,b_]:=a.b-b.a;

	(* now explicitely construct ladder operators for su(3) *)
	(* and from them the actual representations *)

	Block[{J1min,J2min,J3min,J1plu,J2plu,J3plu,J1z,J2z,J3z},
	
		J1min = GenerateLadderMatrix[1,irrep];
		J2min = GenerateLadderMatrix[2,irrep];
		J1plu = ConjugateTranspose[J1min];
		J2plu = ConjugateTranspose[J2min];

		J3plu = com[J1plu,J2plu];
		J3min = ConjugateTranspose[J3plu];

		J1z = 1/2 com[J1plu,J1min];
		J2z = 1/2 com[J2plu,J2min];
		J3z = 1/2 com[J3plu,J3min];

		t = Table[0,{8}];
		t[[1]] = Simplify[1/2(J1plu+J1min)];
		t[[2]] = Simplify[1/(2I) * (J1plu-J1min)];
		t[[3]] = Simplify[1/2(J3plu+J3min)];
		t[[4]] = Simplify[1/(2I) * (J3plu-J3min)] ;
		t[[5]] = Simplify[1/2(J2plu+J2min)];
		t[[6]] = Simplify[1/(2I) * (J2plu-J2min)];
		t[[7]] = Simplify[J1z];
		t[[8]] = Simplify[1/(2*Sqrt[3])*(2*J2z+2*J3z)];

		(* insert 7th entry to 3rd entry, to match literature standard *)
		t[[{3,4,5,6,7}]] = t[[{7,3,4,5,6}]];
	];

	Return[t];
]


(* Generates the indl-th ladder matrix of an irrep (specified in GT-language) *)
GenerateLadderMatrix[indl_, irrepPattern_] := Block[{l=indl,irrep=irrepPattern,p,dim,i,j,k,matrix},
	
	p = GenerateGTPattern[irrep]; (* generate all possible GT patterns for this irrep *)
	(* the patterns correspond to the weight vectors (no multiplicities!) *)

	dim = Length[p]; (* dimension of the carrier space *)
	
	matrix = Table[0,{dim},{dim}];

	For[i=dim,i>= 1,i--,
		For[j=dim,j>= 1,j--,
			k = ComparePattern[l,p[[i]],p[[j]]];
	
			If[ k!=0,
				matrix[[i,j]]=J[p[[i]],k,l];
			];
		];
	];

	Return[matrix];
]

(* Compare two patterns and check whether they are connected by indl-th ladder operators *)
(* if they are not connected, the function returns 0, otherwise *)
(* it returns the index of the changed column. *)
ComparePattern[indl_,pattern1_,pattern2_]:=Block[{l=indl,p1=pattern1,p2=pattern2,k},
	k=0;

	If[l==1,

		If[p1[1,2]==p2[1,2]  && p1[2,2]==p2[2,2] && p1[1,1]==p2[1,1]+1,
			k=1;
		,
			k=0;
		]

	] If[l==2,

		If [p1[1,1]!=p2[1,1], k=0,

			If[p1[1,2]==p2[1,2] && p1[2,2] == (p2[2,2]+1),
				k=2;
			];
			
			If[p1[2,2]==p2[2,2] && p1[1,2]==(p2[1,2]+1),
				k=1;
			];
		];

	];

	Return[k];
]

(* Generate all possible su(3) GT patterns for an irrep 'irrepPattern' *)
(* By construction there are no multiplicities and the length of the list *)
(* is the dimension of the irrep *)
GenerateGTPattern[irrepPattern_]:=Block[{irrep=irrepPattern,list,i,j,k},
	list={};(* list of all GT patterns *)

	For[i=irrep[[3]], irrep[[3]] <= i <= irrep[[2]], i++, (* upper right *)
		For[j=irrep[[2]], irrep[[2]]<= j <= irrep[[1]], j++, (* upper left *)
			For[k=i, i <= k <= j, k++, (* bottom *)
				Module[{m},
					m[1,3]=irrep[[1]];
					m[2,3]=irrep[[2]];
					m[3,3]=irrep[[3]];
					m[1,2]=j;
					m[2,2]=i;
					m[1,1]=k;
					list=Append[list,m];
				];
			];
		];
	];

	Return [list];
];


(* Nonzero matrix elements of the lth ladder operator
	<m-m^kl|J_(l)|m>
	Formula by Gelfand, Tsetlin *)
J[m_,k_,l_]:=Sqrt[-Product[m[\[Kappa],l+1]-m[k,l]+k-\[Kappa]+1,{\[Kappa],1,l+1}]*
Product[m[\[Kappa],l-1]-m[k,l]+k-\[Kappa],{\[Kappa],1,l-1}]/
Product[If[\[Kappa]!= k,(m[\[Kappa],l]-m[k,l]+k-\[Kappa]+1)(m[\[Kappa],l]-m[k,l]+k-\[Kappa]),1],{\[Kappa],1,l}]];


End[];
EndPackage[];