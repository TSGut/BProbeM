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



BeginPackage["BProbe`"];

(* The sole PUBlIC API of this package *)
(***************************************************************************************)

	ProbeInit::usage = "ProbeInit[list] expects a list of d matrices. This method takes care of building the appropriate (Laplace-/Dirac-) operator for you which is needed to perform the rasterizing procedure. Note that calling the method ProbeInit[list] is absolutely required before other methods of this package can be used!
For more information see the User Manual. TODO link";

	ProbeScan::usage = "ProbeScan[dimension , stepsize] performs the actual scanning procedure. It implements an algorithm to rasterize the semi-classical limit of the brane configuration defined by a set of matrices as submanifold of the target space.
For more information see the User Manual. TODO link";
	
	ProbeGetPointList::usage = "ProbeGetPointList[] returns the List[] of already calculated points. A point is itself represented as a List[] consisting of d numbers.
For more information see the User Manual. TODO link";
	
	ProbeReset::usage = "ProbeReset[] resets the package in a way, so that the command ProbeScan[dimension , stepsize] starts a completely new calculation.
For more information see the User Manual. TODO link";

	ProbeGetMinEigenvalue::usage = "ProbeGetMinEigenvalue[point] returns the minimal eigenvalue (with respect to the modulus) of the (Laplace-/Dirac-) operator in question for a given point.
For more information see the User Manual. TODO link";

	ProbeGetEigenvalues::usage = "ProbeGetEigenvalues[point] returns the eigenvalues of the (Laplace-/Dirac-) operator in question for a given point.
For more information see the User Manual. TODO link";
	
	ProbeGetState::usage = "ProbeGetState[point] returns the eigenvector corresponding to the minimal eigenvalue (with respect to the modulus) of the (Laplace-/Dirac-) operator in question for a given point.
For more information see the User Manual. TODO link";

(***************************************************************************************)


Begin["`Private`"];
	Get[ "BProbe`Walk`" ];
	Get[ "BProbe`Gamma`" ];
	

	Options[ProbeInit] = Options[BProbe`Walk`init] ~Join~ {Probe -> "Laplace", Subspace -> Full};
	ProbeInit[t_, opts:OptionsPattern[]] := Block[{p,dim,n,m,expr,i,gamma},
		
		dim = Length[t];				(* dimension of target space *)
		n = Length[t[[1]]];				(* n times n matrices *)
		
		p = Table[Unique["p"], {dim}];
		If[ListQ[OptionValue[Subspace]],
			p[[Complement[Range[dim],OptionValue[Subspace]]]] = 0;
		];
		
		If[OptionValue[Probe] == "Laplace",
			Print["Building Laplace Operator ..."];
		
			m = Sum[(IdentityMatrix[n] p[[i]] - t[[i]]).(IdentityMatrix[n] p[[i]] - t[[i]]), {i, 1, dim}];
			expr = Simplify[ComplexExpand[m], Element[p, Reals]];
		
		,If[OptionValue[Probe] == "Dirac",
			Print["Building Dirac Operator ..."];
			
			gamma = BProbe`Gamma`MatrixRepGamma[dim];
			m = Sum[KroneckerProduct[gamma[[i]], (t[[i]] - IdentityMatrix[n] p[[i]])], {i, 1, dim}];
			expr = Simplify[ComplexExpand[m], Element[p, Reals]];
			
		]];
		
		cm = Compile @@ {DeleteCases[p,0], expr};
		func[x_] := Abs[Eigenvalues[cm @@ N[x], -1][[1]]];
		
		BProbe`Walk`init[func, DeleteCases[p,0], FilterRules[{opts}, Options[BProbe`Walk`init]]];

	];
	
	Options[ProbeReset] = Options[BProbe`Walk`reset];
	ProbeReset[opts:OptionsPattern[]] := Block[{},
		
		BProbe`Walk`reset[opts];
		
	];

	Options[ProbeScan] = Options[BProbe`Walk`start] ~Join~ {MaxEVRatio->\[Infinity], MaxFunctionValue->\[Infinity], MaxGradient->\[Infinity], LogFile->""};
	ProbeScan[bdim_, stepsize_, opts:OptionsPattern[]] := Block[{result},
	
		result = BProbe`Walk`start[bdim, stepsize,
			OptionValue[MaxFunctionValue],
			OptionValue[MaxEVRatio],
			OptionValue[MaxGradient],
			OptionValue[LogFile],
			FilterRules[{opts}, Options[BProbe`Walk`start]]
		];
		
		(*
		Return[result];
		*)
		(* forget about the result, it can be fetched with ProbeGetPointList[] anyway *)
		Return[];
	];
	
	ProbeGetPointList[] := Block[{},
		Return[BProbe`Walk`getlist[]];
	];
	
	ProbeGetMinEigenvalue[p_] := Block[{},
		Return[Abs[Eigenvalues[cm @@ N[p], -1][[1]]]];
	];
	
	ProbeGetEigenvalues[p_] := Block[{},
		Return[Eigenvalues[cm @@ N[p]]];
	];
	
	ProbeGetState[p_] := Block[{},
		Return[Eigensystem[cm @@ N[p],-1][[2,1]]];
	];
	

End[]

EndPackage[]
