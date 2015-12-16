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

	(* we don't need to expose this, since the user doesn't see it anyhow *)
	init::usage="init";
	start::usage="start";
	reset::usage="reset";
	getlist::usage="getlist";


Begin["`Private`"];

	Get["BProbe`Logger`"];
	Get["BProbe`Queue`"];


	Options[init] = {StartingPoint -> "min"}
	init[f_, exp_, x_, opts:OptionsPattern[]] :=
		Block[{s,f2},

			func = f;
			expvfunc = exp;

			If[!ListQ[OptionValue[StartingPoint]],

				Print["Look for global minimum ..."];
				f2[p__?NumericQ] := Abs[func[{p}]];
				s = NMinimize[f2 @@ x,x];
				startPoint = x /. s[[2]];
				
			,
				startPoint = OptionValue[StartingPoint];
			];
			
			reset[opts];

			Print["---"];

			Print["Minimum / Starting point:\n", func[startPoint], " at ", startPoint];
			Print["Gradient:\n", NGradient[func,startPoint]];
			Print["Abs Hess. Eigv:\n", Sort[Abs[#]&/@ Eigenvalues[NHessian[func,startPoint, Scale -> 0.01]]]];
			
			inited=True; (* say: okay, we did a initialization *)
		];


	Options[reset] = Options[init];
	reset[OptionsPattern[]] := Block[{},
		
			If[ListQ[OptionValue[StartingPoint]],
				startPoint = OptionValue[StartingPoint];
			]; 
		
			(* init stuff *)
			pointlist = {};
			AppendTo[pointlist, startPoint];
			boundary = new[Queue];
			boundary.push[{startPoint,startPoint}];
		
			rejectedCounterGrad = 0;
			rejectedCounterVal = 0;
			rejectedCounterRat = 0;
			maxFuncValTracker = 0;
			maxEVRatioTracker = 0;
			maxGradientTracker = 0;
	];
	
	
	getlist[] := Return[pointlist];


	Options[start] = {MinimalSurface -> False, MaxEVRatio->\[Infinity], MaxDisplacementEnergy->\[Infinity], MaxGradient->\[Infinity], ReplacePoints->True, UpdateInterval->0.1, LogFile->""}
	start[numberld_, ssize_, opts:OptionsPattern[]] := (* [number of directions, step qsize] *)
		Block[{ppoint, cpoint, npoints, minpos, m, i},
		
			step = ssize;
			numberldirs = numberld;
			
			startOptions = opts;
			
			
			(* init call is necessary! *)
			If[inited==False,
				Message["First call Walk`init with appropriate parameters ... "];
				Abort[];
			];

			logger = new[Logger, OptionValue[LogFile]];


			cpoint = Last[pointlist];
			ppoint = Last[pointlist];


			(* CORE *)
			Monitor[
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
			
			,	
				(* status message *)
				Refresh[ generateStatus[], TrackedSymbols->{}, FilterRules[Options[start], Options[Refresh]]]
			];
			
			close[logger];
			
			(* print it out again, so it doesnt just vanish *)
			Print[generateStatus[]];
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
		Block[{nhess, directions, processed},
			
			nhess = NHessian[func, point, Scale -> step/10];
			
			
			(* This should actually be checked in the "QValidDirection" method, but *)
			(* then the hessian would have to be recalculated.. so for performance reasons ... *)
			If[QEVRatioTooHigh[nhess],
				log[logger, "point rejected (evratiotoohigh) -" <> TextString[TextString[point]]];
				rejectedCounterRat += 1;
				
				Return[{}];
			];
			
			(* directions from Hessian *)
			directions = Eigensystem[nhess, -numberldirs][[2]];
			
			
			(* double them (forward, backward) *)
			processed = {};
			For[i=1, i<=Length[directions], i++,
				AppendTo[processed, directions[[i]]];
				AppendTo[processed, -directions[[i]]];
			];
			
			
			Return[processed];
		];


	manipulatePoints[npoints_] :=
		Block[{manpoints, i, p, f2, s},
			
			manpoints = npoints;
		
			(* if the surface is a minimum, we can apply *)
			(* FindMinimum to get a better approximation *)		
			If[opts[MinimalSurface],
			
				manpoints = {};
				
				f2[p__?NumericQ] := func[{p}];
				p = Table[Unique["p"], {Length[npoints[[1]]]}];
				
				For[i=1, i<=Length[npoints], i++,
					
					Quiet[s = FindMinimum[f2 @@ p, Thread[{p,npoints[[i]]}]]];
					(* , MaxIterations->5 *)
					
					(* processed = ReplacePart[processed, i -> ((p /. s[[2]]) - point)]; *)
					AppendTo[manpoints, (p /. s[[2]])];
				];
			];
			
			(* if not deactivated *)
			(* replace points by their corresponding expectation values *)
			If[opts[ReplacePoints],
				manpoints = expvfunc[#]& /@ npoints;
			];
			
			
			Return[manpoints];
			
		];


	filterPoints[ppoint_,npoints_] := (* [pastpoint, newpoints] *)
		Block[{filtered, i},

			filtered = {};

			For[i=1, i<=Length[npoints], i++,

				If[QValidPoint[ppoint, npoints[[i]]],
					(* add *)
					AppendTo[filtered, npoints[[i]]];
				];

			];

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
							TextString[func[npoint]] <> "-" <> TextString[npoint] <> "-" <> TextString[ppoint]];
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
			evs = Eigenvalues[nhess, -(numberldirs+1)];
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
			
			(* test *)
			grad = NGradient[func, point];
			
			If[Norm[grad] > maxGradientTracker, maxGradientTracker = Norm[grad]];
			
		
			(* perform check only if opts[MaxGradient] is finite *)
			If[opts[MaxGradient] < \[Infinity],
				
				
				If[Norm[grad] < opts[MaxGradient],
					Return[False];
				,
					Return[True];
				];
				
			,
				Return[False];
			];

		];


	QEnergyTooHigh[point_]:=
		Block[{val},
			
			val = Abs[func[point]];
		
			(* test *)
			If[val > maxFuncValTracker, maxFuncValTracker = val];
		
			(* perform check only if opts[MaxDisplacementEnergy] is finite *)
			If[opts[MaxDisplacementEnergy] < \[Infinity],
				
				If[val < opts[MaxDisplacementEnergy],
					Return[False];
				,
					Return[True];
				];
			
			,
				Return[False];
			];
		];


	(* Are we going back again? *)
	QBack[ppoint_,npoint_]:= (* [pastpoint, newpoint] *)
		Norm[npoint-ppoint] < step*0.7;		(* TODO: check if this makes sense in all poss. configs *)


	QNearPoints[point_]:= (* [point] *)
		Block[{near},

			near = Nearest[pointlist,point][[1]];

			If[Norm[point-near] < step*0.3,
				Return[True];
			,
				Return[False];
			];

		];
		
	opts[symbol_] := OptionValue[start, startOptions, symbol];
	
	generateStatus[] :=
		Block[{status},
			status = {
				{ "Total points gathered" , Length[pointlist] },
				{ "Points in queue" , size[boundary] },
				{ "Max occured EV-Ratio" , maxEVRatioTracker },
				{ "Max occured displacement energy" , maxFuncValTracker },
				{ "Max occured gradient" , maxGradientTracker }
			};
			
			If[opts[MaxEVRatio] < \[Infinity],
				AppendTo[status, {"Rejected pts (EVRatio)" , rejectedCounterRat }];
			];
			
			If[opts[MaxDisplacementEnergy] < \[Infinity],
				AppendTo[status, { "Rejected pts (FuncValue)" , rejectedCounterVal }];
			];
			
			If[opts[MaxGradient] < \[Infinity],
				AppendTo[status, { "Rejected pts (Gradient)" , rejectedCounterGrad }];
			];
			
			Panel[TextGrid[
				status,
				Dividers -> Center,
				Alignment -> {{Left,Right}},
				Spacings -> {3,2}
			]]
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
