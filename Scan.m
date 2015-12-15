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
	];
	
	
	getlist[] := Return[pointlist];


	Options[start] = {MinimalSurface -> False}
	start[numberld_, ssize_, maxv_, maxevr_, gradtolf_, logfilename_, OptionsPattern[]] := 
		(* [number of directions, step size, tol factor for function value, ratio tol factor, tol factor for gradient, filename of log file] *)
		Block[{ppoint, cpoint, npoints, minpos, m, i},

			step = ssize;
			numberldirs = numberld;

			minsurf = OptionValue[MinimalSurface];
			maxevratio = maxevr;
			maxval = maxv;
			gradtolfactor = gradtolf;



			(* init call is necessary! *)
			If[inited==False,
				Message["First call Walk`init with appropriate parameters ... "];
				Abort[];
			];

			logger = new[Logger,logfilename];			


			cpoint = Last[pointlist];
			ppoint = Last[pointlist];


			(* CORE *)
			Monitor[Monitor[Monitor[Monitor[Monitor[
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
			, "Points at boundary: " <> TextString[size[boundary]]]
			, "Rejected points (Gradient): " <> TextString[rejectedCounterGrad]]
			, "Rejected points (FuncValue): " <> TextString[rejectedCounterVal]]
			, "Rejected points (EVRatio): " <> TextString[rejectedCounterRat]]
			, "Added points: " <> TextString[Length[pointlist]]];


			close[logger];
			Return[pointlist];
		];


	doStep[ppoint_,cpoint_]:= (* [pastpoint, currentpoint] *)
		Block[{npoints,dirs},
		
			dirs = determineDirections[cpoint];
			dirs = processDirections[cpoint, dirs*step];
			dirs = filterDirections[ppoint, cpoint, dirs];

			npoints = (cpoint + #)& /@ dirs;

			Return[npoints];
		];


(* PRIVATE METHODS (informal) *)

	determineDirections[point_]:= (* [point, tolerance] *)
		Block[{nhess, directions},
			
			nhess = NHessian[func, point, Scale -> step/10];
			
			
			(* This should actually be checked in the "QValidDirection" method, but *)
			(* then the hessian would have to be recalculated.. so for performance reasons ... *)
			If[QEVRatioTooHigh[nhess],
				log[logger, "point rejected (evratiotoohigh) -" <> TextString[TextString[point]]];
				rejectedCounterRat += 1;
				
				Return[{}];
			];
			
			
			directions = Eigensystem[nhess, -numberldirs][[2]];

			Return[directions];
		];


	processDirections[point_,directions_] :=
		Block[{processed, i, npoint, p, f2, s},
			
			processed = {};
			
			For[i=1, i<=Length[directions], i++,
				AppendTo[processed, directions[[i]]];
				AppendTo[processed, -directions[[i]]];
			];


		(* if the surface is a minimum, we can apply FindMinimum to get a better approximation *)		
			If[minsurf,
			
				f2[p__?NumericQ] := func[{p}];
				For[i=1, i<=Length[processed], i++,
					
					npoint = point + processed[[i]];
					p = Table[Unique["p"], {Length[point]}];
				
					Quiet[s = FindMinimum[f2 @@ p, Thread[{p,npoint}]]];
					(* , MaxIterations->5 *)
					
					processed = ReplacePart[processed, i -> ((p /. s[[2]]) - point)];
				
				];
			];
			

			Return[processed];
			
		];


	filterDirections[ppoint_,cpoint_,directions_] := (* [pastpoint, currentpoint, directions] *)
		Block[{filtered, i},

			filtered = {};

			For[i=1, i<=Length[directions], i++, (* for each direction *)

				If[QValidDirection[ppoint, cpoint, directions[[i]], directions],
					(* add *)
					AppendTo[filtered, directions[[i]]];
				];

			];

			Return[filtered];
		];


	norm[point_]:= (* [point] *)
		point/Norm[point]


	QValidDirection[ppoint_,cpoint_,dir_,dirs_]:= (* [pastpoint, currentpoint, direction] *)
		Block[{npoint},
			npoint = cpoint + dir;

				If[Not[QBack[ppoint, npoint]],
					If[Not[QValueTooHigh[npoint]],
						If[Not[QGradientTooHigh[npoint,dirs]],
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
		
		
			(* perform check only if evratio is finite *)
			If[maxevratio < \[Infinity],
		
				evs = Eigenvalues[nhess, -(numberldirs+1)];
				ratio = evs[[2]]/evs[[1]];
			
				If[ratio < maxevratio,
					Return[False];	
				,
					Return[True];
				];
			
			,
				Return[False];
			];
		
		];


	QGradientTooHigh[point_,dirs_]:= (* [point] *)
		Block[{grad, proj},
			
			(* perform check only if gradtolfactor is finite *)
			If[gradtolfactor < \[Infinity],
				
				grad = NGradient[func, point];
	
				If[Norm[grad] < gradtolfactor,
					Return[False];
				,
					Return[True];
				];
				
			,
				Return[False];
			];

		];


	QValueTooHigh[npoint_]:=
		Block[{},
			
			(* perform check only if maxval is finite *)
			If[maxval < \[Infinity],
				
				If[Abs[func[npoint]] < maxval,
					Return[False];
				,
					Return[True];
				];
			
			,
				Return[False];
			];
		];


	(* Are we going back again? *)
	QBack[ppoint_,npoint_]:= (* [pastpoint, currentpoint, dir] *)
		Block[{},

			(* TODO: check if this makes sense in all poss. configs *)

			If[Norm[npoint-ppoint] < step*0.7,
				Return[True];
			,(*else*)
				Return[False];
			];

		];


	QNearPoints[point_]:= (* [point] *)
		Block[{near},

			near = Nearest[pointlist,point][[1]];

			If[Norm[point-near] < step*0.7,
				Return[True];
			,
				Return[False];
			];

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
