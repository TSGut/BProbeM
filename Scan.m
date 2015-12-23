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


Begin["`Private`"];

	Get["BProbe`Logger`"];
	Get["BProbe`Queue`"];


	Options[init] = {
		StartingPoint -> "min",
		Probe -> "Laplace",
		Subspace -> Full
	};
	init[t_, opts:OptionsPattern[]] :=
		Block[{info, evlist, i, subspace, obs, gdim},

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
			, If[OptionValue["Probe"] == "Dirac" || OptionValue["Probe"] == "DiracSq",
				gdim = Length[MatrixRepGamma[Length[t]][[1]]];
				obs = KroneckerProduct[IdentityMatrix[gdim],#]& /@ t;
			]];
			cexp = buildExpectationValue[obs[[subspace]]];

			energyf[y_] := Abs[Eigenvalues[cop @@ N[y], -1][[1]]];
			expvfunc[y_] := Re[cexp @@ getState[N[y]]];
			
			(* automatically determine starting point, if not given *)
			If[!ListQ[OptionValue[StartingPoint]],
				Block[{f2,p,s},
					PrintTemporary["* Look for global minimum of energy displacement ..."];
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
			evlist = Sort[Abs[#]&/@ Eigenvalues[NHessian[func,startPoint, Scale -> 0.01]]];
			For[i=1,i<=Length[evlist],i++,
				If[evlist[[i]] < 0.3,
					branedim += 1;
					evlist[[i]] = Style[evlist[[i]], Darker[Green]]; (* and colorize for later *)
				,
					evlist[[i]] = Style[evlist[[i]], Darker[Red]];
				];
			];
			
			reset[opts];
			
			(* print info *)
			info = {
				{ "Energy Probe", Style[OptionValue[Probe], Bold] },
				{ "Starting Point (SP)", MatrixForm[startPoint] },
				{ "Energy at SP" , TextString[energyf[startPoint]] },
				{ "Norm of Gradient at SP" , Norm[NGradient[func,startPoint]] },
				{ "Absolute Hessian Eigenvalues at SP", MatrixForm[evlist] },
				{ "Local brane dimension", Style[ToString[branedim],{Darker[If[branedim==0,Red,Green]],Bold}] }
			};
			
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
			pointlist = {};
			AppendTo[pointlist, startPoint];
			boundary = new[Queue];
			boundary.push[{startPoint,startPoint}];
		
			rejectedCounterGrad = 0;
			rejectedCounterVal = 0;
			rejectedCounterRat = 0;
			maxEnergyTracker = 0;
			maxEVRatioTracker = 0;
			maxGradientTracker = 0;
	];
	
	
	getList[] := Return[pointlist];
	getMinEigenvalue[p_] := Abs[Eigenvalues[cop @@ N[p], -1][[1]]];
	getEigenvalues[p_] := Eigenvalues[cop @@ N[p]];
	getState[p_] := Eigenvectors[cop @@ N[p],-1][[1]];

	
	Options[start] = {
		Dimension -> branedim,
		MinimalSurface -> False,
		GradientTracker -> False,
		EnergyTracker -> False,
		MaxEVRatio->\[Infinity],
		MaxEnergy->\[Infinity],
		MaxGradient->\[Infinity],
		ReplacePoints->True,
		LogFile->"",
		Profiling->False
	}
	start[ssize_, opts:OptionsPattern[]] := (* [step size] *)
		Block[{ppoint, cpoint, npoints, minpos, m, i},

			step = ssize;
			startOptions = opts;
			logger = new[Logger, OptionValue[LogFile]];

			cpoint = Last[pointlist];
			ppoint = Last[pointlist];


			(* CORE *)
			While[boundary.size[] != 0,
				{ppoint, cpoint} = boundary.pop[];
				
				
				npoints = doStep[ppoint, cpoint];
				
				(* append new points, boundary info  + ppoints *)
				boundary.pushList[Thread[{
					Table[cpoint,{Length[npoints]}] ,
					npoints
				}]];
				pointlist = Join[pointlist, npoints];
				
				log[logger,
					"point accepted -" <> TextString[#]
				]& /@ npoints;
				
			];
			
			close[logger];
			
		];


	doStep[ppoint_,cpoint_]:= (* [pastpoint, currentpoint] *)
		Block[{npoints,dirs},
		
			dirs = determineDirections[cpoint];
			
			npoints = (cpoint + #*step)& /@ dirs;
			npoints = manipulatePoints[ npoints ];
			npoints = filterPoints[ppoint, npoints];

			Return[npoints];
		];

		
		

(* PRIVATE METHODS (informal) *)

	determineDirections[point_]:= (* [point, tolerance] *)
		Block[{nhess, directions},
			
			(nhess = NHessian[func, point, Scale -> step/10])	 ~rec~ "Hessian" ;
			
			(* This should actually be checked in the "QValidDirection" method, but *)
			(* then the hessian would have to be recalculated.. so for performance reasons ... *)
			If[QEVRatioTooHigh[nhess],
				log[logger, "point rejected (evratiotoohigh) -" <> TextString[TextString[point]]];
				rejectedCounterRat += 1;
				
				Return[{}];
			];
			
			(* directions from Hessian *)
			(directions = Eigenvectors[nhess, -opts[Dimension]])	~rec~ "Eigenvectors";
			
			(* double them (forward, backward) *)
			directions = Riffle[directions, -directions];
			
			Return[directions];
		];


	manipulatePoints[npoints_] :=
		Block[{manpoints, i, p, f2, s},
			
			manpoints = npoints;
		
			(* if the surface is a minimum, we can apply *)
			(* FindMinimum to get a better approximation *)		
			If[opts[MinimalSurface],
				
				f2[p__?NumericQ] := energyf[{p}];
				p = Table[Unique["p"], {Length[npoints[[1]]]}];
				
				manpoints = Flatten[Reap[
				For[i=1, i<=Length[npoints], i++,
					
					(Quiet[s = FindMinimum[f2 @@ p, Thread[{p,npoints[[i]]}]]])  ~rec~ "FindMinimum";
					(* , MaxIterations->5 *)
					
					Sow[(p /. s[[2]])];
				]
				][[2]],1];
			];
			
			(* if not deactivated *)
			(* replace points by their corresponding expectation values *)
			If[opts[ReplacePoints],
				(manpoints = expvfunc[#]& /@ npoints)	~rec~"ReplacePoints";
			];
			
			
			Return[manpoints];
			
		];


	filterPoints[ppoint_,npoints_] := (* [pastpoint, newpoints] *)
		Block[{filtered},

			filtered = Flatten[Reap[
				(If[QValidPoint[ppoint, #], Sow[#]])& /@ npoints;
			][[2]],1];
			
			Return[filtered];
		];


	QValidPoint[ppoint_,npoint_]:= (* [pastpoint, newpoint] *)
		Block[{},

			If[Not[QBack[ppoint, npoint]],
				If[Not[QEnergyTooHigh[npoint]],
					If[Not[QGradientTooHigh[npoint]],
						If[Not[QNearPoints[npoint]],
							Return[True];
						,
							log[logger,
								"point rejected (nearpoints) -" <>
								TextString[npoint] <> "-" <> TextString[ppoint]];
						];
					,
						log[logger,
							"point rejected (gradienttoohigh) -" <>
							TextString[npoint] <> "-" <> TextString[ppoint]];
						rejectedCounterGrad += 1;
					];
				,
					log[logger,
						"point rejected (valuetoohigh) -" <>
						TextString[energyf[npoint]] <> "-" <> TextString[npoint] <> "-" <> TextString[ppoint]];
					rejectedCounterVal += 1;
				];
			,
				log[logger,
					"point rejected (back) -" <>
					TextString[npoint] <> "-" <> TextString[ppoint]];
			];

			(*otherwise*)
			Return[False];
		];


	QEVRatioTooHigh[nhess_] := (* [hesse matrix] *)
		Block[{evs, ratio},
		
			(* test *)
			evs = Eigenvalues[nhess, -(opts[Dimension]+1)];
			ratio = evs[[2]]/evs[[1]];
			
			If[ratio > maxEVRatioTracker, maxEVRatioTracker = ratio];
		
		
			(* perform check only if evratio is finite *)
			If[opts[MaxEVRatio] < \[Infinity],
		

			
				If[ratio < opts[MaxEVRatio],
					Return[False];	
				,
					Return[True];
				];
			
			,
				Return[False];
			];
		
		];


	QGradientTooHigh[point_]:= (* [point] *)
		Block[{grad},
			
			(* perform check only if opts[MaxGradient] is finite *)
			(* or opts[GradientTracker] is set *)
			If[opts[MaxGradient] < \[Infinity] || opts[GradientTracker],
				
				grad = NGradient[func, point]	~rec~"Gradient";
				
				If[Norm[grad] > maxGradientTracker, maxGradientTracker=Norm[grad]];

				Return[Norm[grad] > opts[MaxGradient]];
			,
				Return[False];
			];

		];


	QEnergyTooHigh[point_]:=
		Block[{val},
			
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


	QNearPoints[point_]:= (* [point] *)
		Block[{near},

			near = Nearest[pointlist,point,{1,step*0.3}]	~rec~"NNS" ;
			
			Return[Length[near] != 0];
			
		];
		
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
NHessian[f_,x_?(VectorQ[#,NumericQ]&),opts___?OptionQ] :=
	Block[{n,h,norm,z,mat,f0},
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


NGradient[f_,x_?(VectorQ[#,NumericQ]&),opts___?OptionQ] :=
	Block[{n,h,norm,z,mat,f0},
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
