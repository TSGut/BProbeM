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



BeginPackage["BProbe`Scan`"];

	Get["BProbe`Profiler`"]
	Get["BProbe`Gamma`"];

	(* we don't need to expose this, since the user doesn't see it anyhow *)
	init::usage="init";
	start::usage="start";
	reset::usage="reset";
	getList::usage="getlist";
	getMinEigenvalue::usage="";
	getEigenvalues::usage="";
	getState::usage="";
	getExpectedLocation::usage="";


Begin["`Private`"];

	Options[init] = {
		StartingPoint -> "min",
		Probe -> "Laplace",
		Subspace -> Full
	};
	init[t_, opts:OptionsPattern[]] := Block[{info, evlist, subspace, obs, gdim, hdim},
		
		If[Not[ListQ[OptionValue[Subspace]]],
			(* in this case, take the full space *)
			subspace = Range[1, Length[t]];
		,
			subspace = OptionValue[Subspace];
		];
	
		PrintTemporary["* Compiling Operator ..."];
		cop = buildOperator[t, subspace, OptionValue["Probe"]];
		
		PrintTemporary["* Compiling expectation-value function ..."];
		If[OptionValue["Probe"] == "Laplace",
			obs = t;
			hdim = Length[t[[1]]];
		, If[OptionValue["Probe"] == "Dirac" || OptionValue["Probe"] == "DiracSq",
			gdim = Length[MatrixRepGamma[Length[t]][[1]]];
			obs = KroneckerProduct[IdentityMatrix[gdim],#]& /@ t;
			hdim = Length[t[[1]]] * gdim;
		]];
		cexp = buildExpectationValue[obs[[subspace]]];

		energyf[y_] := Abs[Eigenvalues[cop @@ N[y], -1][[1]]];
		expvfunc[y_] := Re[cexp @@ getState[N[y]]];
		
		(* automatically determine starting point, if not given *)
		If[!ListQ[OptionValue[StartingPoint]],
			Block[{f2,p,s},
				PrintTemporary["* Look for global minimum of displacement energy ..."];
				p = Table[Unique["p"], {Length[t]}];
				p[[Complement[Range[Length[t]],subspace]]] = 0;
				p = DeleteCases[p,0];
				f2[p__?NumericQ] := energyf[{p}];
				s = NMinimize[f2 @@ p,p];
				startPoint = p /. s[[2]];
			];
		,
			startPoint = OptionValue[StartingPoint];
		];
		
		(* automatically determine local dimension of brane *)
		(* i.e. just check for eigenvalues < some small value *)
		branedim = 0;
		evlist = Sort[Abs[#]&/@ Eigenvalues[NHessian[energyf,startPoint, Scale -> 0.01]]];
		Scan[(
			If[# < 0.3,
				branedim += 1;
			];
		)&, evlist];
		
		(* guess some step size (at least of the correct order) *)
		Block[{evs, vol,volpp},
			vol = Times @@ Map[(
				evs = Re[Eigenvalues[#]]; (* Re: to be sure *)
				(Max[evs]-Min[evs])
			)&, N[t[[subspace]]]];
			volpp = vol / 3000;
			stepsizeguess = Power[volpp,1/Length[subspace]];
		];
		
		reset[opts];
		
		info = <|
			"EnergyProbe" -> OptionValue[Probe],
			"StartingPoint" -> startPoint,
			"EnergySP" -> energyf[startPoint],
			"GradientSP" -> Norm[NGradient[energyf,startPoint]],
			"HEigenvaluesSP" -> evlist,
			"BraneDimension" -> branedim,
			"TargetSpaceDimension" -> Length[subspace],
			"HilbertSpaceDimension" -> hdim,
			"StepSize" -> stepsizeguess
		|>;
		
		Return[info];
	];


	buildOperator[t_, subspace_, probe_] := Block[{n,dim,p,m,expr,cm,gamma},
		dim = Length[t];
		n = Length[t[[1]]];
	
		p = Table[Unique["p"], {dim}];
		p[[Complement[Range[dim],subspace]]] = 0;
		
		Switch[probe,
		"Laplace",
			m = Sum[(IdentityMatrix[n] p[[i]] - t[[i]]).(IdentityMatrix[n] p[[i]] - t[[i]]), {i, 1, dim}];
	
		,"Dirac",
			gamma = BProbe`Gamma`MatrixRepGamma[dim];
			m = Sum[KroneckerProduct[gamma[[i]], (t[[i]] - IdentityMatrix[n] p[[i]])], {i, 1, dim}];
			
		,"DiracSq",
			gamma = BProbe`Gamma`MatrixRepGamma[dim];
			m = Sum[KroneckerProduct[gamma[[i]], (t[[i]] - IdentityMatrix[n] p[[i]])], {i, 1, dim}];
			m = m.m;
		];
		
		expr = ComplexExpand[m];
		cm = Compile @@ {DeleteCases[p,0], expr, RuntimeOptions->"Speed", CompilationTarget->"C"};
		
		Return[cm];
	];

	buildExpectationValue[obs_] := Block[{n,expr,x,cexp},
		n = Length[obs[[1]]];
		
		x = Table[Unique["x"], n];
		expr = ComplexExpand[(Conjugate[x].#.x)& /@ obs, x];
		cexp = Compile @@ {Thread[{x, Table[_Complex, Length[x]]}], expr, RuntimeOptions->"Speed", CompilationTarget->"C"};
		
		Return[cexp];
	];

	Options[reset] = Options[init];
	reset[OptionsPattern[]] := Block[{},
		
		If[ListQ[OptionValue[StartingPoint]],
			startPoint = OptionValue[StartingPoint];
		]; 
	
		(* init stuff *)
		ResetProfile[];
		pointlist = {startPoint};
		
		boundary = {{1,1}};
	
		rejectedCounterGrad = 0;
		rejectedCounterVal = 0;
		rejectedCounterRat = 0;
		maxEnergyTracker = 0;
		maxEVTracker = 0;
		maxGradientTracker = 0;
		
	];
	
	
	getList[] := Return[pointlist];
	getMinEigenvalue[p_] := Abs[Eigenvalues[cop @@ N[p], -1][[1]]];
	getEigenvalues[p_] := Eigenvalues[cop @@ N[p]];
	getState[p_] := Eigenvectors[cop @@ N[p],-1][[1]];
	getExpectedLocation[state_] := Re[cexp @@ state];

	
	Options[start] = {
		Dimension -> branedim,
		StepSize -> stepsizeguess,
		MinimalSurface -> False,
		GradientTracker -> False,
		EnergyTracker -> False,
		EVTracker -> False,
		MaxEV->\[Infinity],
		MaxEnergy->\[Infinity],
		MaxGradient->\[Infinity],
		ReplacePoints->True,
		Parallelize->True,
		Profiling->False
	}
	start[opts:OptionsPattern[]] := Block[{ppoint, cpoint},

		step = OptionValue[StepSize];
		startOptions = opts;

		If[OptionValue[Parallelize],
			Quiet[LaunchKernels[]];
			DistributeDefinitions[getState, energyf, expvfunc, NHessian, NGradient];
			DistributeDefinitions[step];
		];

		(* CORE *)
		While[Length[boundary] != 0, Block[{dirs, npoints, nearf},
			
			(* determine 'small' directions and double them (forward, backward) *)
			dirs = Riffle[#, -#]& /@ determineDirections[pointlist[[Thread[boundary][[2]]]]];
			
			(* gather all potential new points *)
			(*---------------------------------------------*)
			npoints = Flatten[Reap[Do[Block[{cpoint},
				cpoint = pointlist[[boundary[[i,2]]]];
				Sow[{ i, (cpoint + #*step) }]& /@ dirs[[i]]
			], {i, Length[dirs]}]][[2]],1] ~rec~ "Gathering new points";
			
			(* manipulate new points *)
			(*---------------------------------------------*)
			If[Length[npoints]>0,
				npoints = Thread[{Thread[npoints][[1]], manipulatePoints[ Thread[npoints][[2]] ]}];
			];
			
			(* filter all points: qback, qenergy *)
			(*---------------------------------------------*)
			npoints = Map[Block[{ppoint, cpoint, npoint},
				npoint = #[[2]];
				cpoint = pointlist[[boundary[[#[[1]]]][[2]]]];
				ppoint = pointlist[[boundary[[#[[1]]]][[1]]]];
				
				If[QValidPoint[ppoint, npoint],
					{ #[[1]] , npoint }
				,
					Nothing
				]
			]&,npoints] ~rec~ "Filtering";
			
			(* Gradient *)
			(*---------------------------------------------*)
			Block[{grads},
			If[OptionValue[MaxGradient] < \[Infinity] || OptionValue[GradientTracker],
				grads = If[OptionValue[Parallelize],
					ParallelMap[ NGradient[ energyf, #[[2]] ]&, npoints ] ~rec~"ParallelGradient"
				,
					Map[ NGradient[energyf, #[[2]]]&, npoints ] ~rec~"Gradient"
				];
				
				maxGradientTracker = Max[Max[Norm[#]& /@ grads], maxGradientTracker];
			];
			If[OptionValue[MaxGradient] < \[Infinity],
				npoints = MapIndexed[(
					If[Norm[grads[[First[#2]]]] < OptionValue[MaxGradient],
						#1
					,
						rejectedCounterGrad += 1;
						Nothing
					]
				)&, npoints];
			];
			];
			
			(* NNS - first check on existing points *)
			(*---------------------------------------------*)
			nearf = Nearest[pointlist] ~rec~ "NNS-0";
			npoints = Map[Block[{npoint},
				npoint = #[[2]];
				If[Length[nearf[npoint,{1,step*0.3}]] == 0,
					{ #[[1]] , npoint }
				,
					Nothing
				]
			]&, npoints] ~rec~ "NNS-1";
			
			(* NNS - second check on new points *)
			(*---------------------------------------------*)
			Block[{nnpoints=npoints},
				npoints = {};
				Scan[(
					If[Length[npoints]==0 || Length[Nearest[Thread[npoints][[2]], #[[2]] ,{1,step*0.3}]] == 0,
						AppendTo[npoints, #];
					];
				)&, nnpoints];
			] ~rec~ "NNS-2";
			
			(* shovel all new points into boundary *)
			(*---------------------------------------------*)
			npoints = Map[{ boundary[[#[[1]]]][[2]] , #[[2]] }&, npoints];
			If[Length[npoints] != 0,
				boundary = Thread[{ Thread[npoints][[1]], Length[pointlist] + Range[1,Length[npoints]] }];
				pointlist = Join[pointlist, Thread[npoints][[2]]];
			,
				boundary = {};
			];

		]];
		
	];



(* PRIVATE METHODS (informal) *)

	(* determine directions for a batch of points *)
	determineDirections[points_] := Block[{nhess, directions},
		
		If[opts[Parallelize],
			(nhess = ParallelMap[(
				NHessian[energyf, #, Scale -> step/10]
			)&, points]) ~rec~ "HessianParallel" ;
		,
			(nhess = Map[(
				NHessian[energyf, #, Scale -> step/10]
			)&, points]) ~rec~ "Hessian" ;
		];
		
		(* directions from Hessian *)
		(directions = Map[(
			If[!QEVTooHigh[#],
				Eigenvectors[#, -opts[Dimension]]
			,
				rejectedCounterRat += 1;
				{}
			]
		)&, nhess])	~rec~ "Eigenvectors";
		
		Return[directions];
	];


	manipulatePoints[npoints_] := Block[{manpoints, p, f2},
			
		manpoints = npoints;
	
		(* if the surface is a minimum, we can apply *)
		(* FindMinimum to get a better approximation *)		
		If[opts[MinimalSurface],
			
			f2[p__?NumericQ] := energyf[{p}];
			p = Table[Unique["p"], {Length[npoints[[1]]]}];
			
			If[opts[Parallelize],
				DistributeDefinitions[f2,p];
				manpoints = ParallelMap[Block[{s},
					(Quiet[s = FindMinimum[f2 @@ p, Thread[{p,#}]]]);
					(p /. s[[2]])
				]&, npoints] ~rec~ "FindMinimumParallel";
			,
				manpoints = Map[(
					(Quiet[s = FindMinimum[f2 @@ p, Thread[{p,#}]]]);
					(p /. s[[2]])
				)&, npoints] ~rec~ "FindMinimum";
			];
		];
		
		(* if not deactivated *)
		(* replace points by their corresponding expectation values *)
		If[opts[ReplacePoints],
			If[opts[Parallelize],
				(manpoints = ParallelMap[expvfunc[#]&, npoints])	~rec~ "ReplacePointsParallel";
			,
				(manpoints = expvfunc[#]& /@ npoints)	~rec~"ReplacePoints";
			];
		];
		
		Return[manpoints];
		
	];


	QValidPoint[ppoint_,npoint_] := Block[{},
		
		If[Not[QBack[ppoint, npoint]],
			If[Not[QEnergyTooHigh[npoint]],
				Return[True];
			,
				rejectedCounterVal += 1;
			];
		];

		(*otherwise*)
		Return[False];
	];


	QEVTooHigh[nhess_] := Block[{evs, ratio},
		
		(* perform check only if evratio is finite *)
		If[opts[MaxEV] < \[Infinity] || opts[EVTracker],
			evs = Eigenvalues[nhess, -opts[Dimension]];
			If[evs[[1]] > maxEVTracker, maxEVTracker = evs[[1]]];
			
			Return[evs[[1]] > opts[MaxEV]];
		,
			Return[False];
		];
	
	];
	
	
	QEnergyTooHigh[point_] := Block[{val},
			
		(* perform check only if opts[MaxEnergy] is finite *)
		If[opts[MaxEnergy] < \[Infinity] || opts[EnergyTracker],
			
			val = Abs[energyf[point]]		~rec~"FuncEval";
			
			If[val > maxEnergyTracker, maxEnergyTracker = val];
			
			Return[val > opts[MaxEnergy]];
		,
			Return[False];
		];
	];


	(* Are we going back again? *)
	QBack[ppoint_,npoint_]:= (* [pastpoint, newpoint] *)
		Norm[npoint-ppoint] < step*0.7;		(* TODO: check if this makes sense in all poss. configs *)
	
	
	opts[symbol_] := OptionValue[start, startOptions, symbol];
	
	SetAttributes[rec, HoldFirst];
	rec[expr_,id_] := Block[{ret},
		If[opts[Profiling],
			ret = AddRecord[id, expr];
		,
			ret = expr;
		];
		
		Return[ret];
	];



Options[NHessian]={Scale->10^-3};
NHessian[f_,x_?(VectorQ[#,NumericQ]&),opts___?OptionQ] := Block[{n,h,norm,z,mat,f0},
	n=Length[x];
	h=Scale /. {opts} /. Options[NHessian];
	norm=If[VectorQ[h],Outer[Times,2 h,2 h],4 h^2];
	z=If[VectorQ[h],DiagonalMatrix[h],h*IdentityMatrix[n]];
	mat=ConstantArray[0.,{n,n}];
	f0=f[x];

	Do[
		mat[[i,j]]=
			If[i==j,(*then*)
				.5 (f[x+2*z[[i]]]-2 f0+f[x-2*z[[i]]])
			, (*else*)
				f[x+z[[i]]+z[[j]]]-f[x+z[[i]]-z[[j]]]-f[x-z[[i]]+z[[j]]]+f[x-z[[i]]-z[[j]]]
			];
	,{i,n},{j,i,n}];

	Return[(mat+Transpose[mat])/norm];
];


NGradient[f_,x_?(VectorQ[#,NumericQ]&),opts___?OptionQ] := Block[{n,h,norm,z,mat,f0},
	n=Length[x];
	h=Scale /. {opts} /. Options[NHessian];
	norm=If[VectorQ[h],Outer[Times,Sqrt[2 h],Sqrt[2 h]],2 h];
	z=If[VectorQ[h],DiagonalMatrix[h],h*IdentityMatrix[n]];
	mat=ConstantArray[0.,{n}];
	f0=f[x];

	Do[
		mat[[i]]= f[x-z[[i]]]-f[x+z[[i]]]
	,{i,n}];

	Return[mat/norm];
];


End[];
EndPackage[];
