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



BeginPackage["BProbe`Walk`"];

	(* we don't need to expose this, since the user doesn't see it anyhow *)
	init::usage="init";
	start::usage="start";
	reset::usage="reset";
	getlist::usage="getlist";


Begin["`Private`"];

	Get["BProbe`Logger`"];
	Get["BProbe`Queue`"];


	Options[init] = {StartingPoint -> "min"}
	init[f_,x_, opts:OptionsPattern[]] :=
		Block[{s,f2},

			func = f;

			If[!ListQ[OptionValue[StartingPoint]],

				Print["Look for global minimum ..."];
				f2[p__?NumericQ] := Abs[func[{p}]];
				s = NMinimize[f2 @@ x,x];
				startState = x /. s[[2]];
				
			,
				startState = OptionValue[StartingPoint];
			];
			
			reset[opts];

			Print["---"];

			Print["Minimum / Starting point:\n", func[startState], " at ", startState];
			Print["Gradient:\n", NGradient[func,startState]];
			Print["Abs Hess. Eigv:\n", Sort[Abs[#]&/@ Eigenvalues[NHessian[func,startState, Scale -> 0.01]]]];
			
			inited=True; (* say: okay, we did a initialization *)
		];


	Options[reset] = Options[init];
	reset[OptionsPattern[]] := Block[{},
		
			If[ListQ[OptionValue[StartingPoint]],
				startState = OptionValue[StartingPoint];
			]; 
		
			(* init stuff *)
			states = {};
			AppendTo[states, startState];
			boundary = new[Queue];
			boundary.push[{startState,startState}];
		
			rejectedCounterGrad = 0;
			rejectedCounterVal = 0;
			rejectedCounterRat = 0;
	];
	
	
	getlist[] := Return[states];


	Options[start] = {MinimalSurface -> False}
	start[numberld_, ssize_, maxv_, maxevr_, gradtolf_, logfilename_, OptionsPattern[]] := 
		(* [number of directions, step size, tol factor for function value, ratio tol factor, tol factor for gradient, filename of log file] *)
		Block[{pstate, cstate, nstates, minpos, m, i},

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


			cstate = Last[states];
			pstate = Last[states];


			(* CORE *)
			Monitor[Monitor[Monitor[Monitor[Monitor[
			While[boundary.size[] != 0,
				{pstate, cstate} = boundary.pop[];


				nstates = doStep[pstate, cstate];

				(* append new states, boundary info  + pstates *)
				boundary.pushList[Thread[{
					Table[cstate,{Length[nstates]}] ,
					nstates
				}]];
				states = Join[states, nstates];

				log[logger,
					"state accepted -" <> TextString[#]
				]& /@ nstates;

			];
			, "Points at boundary: " <> TextString[size[boundary]]]
			, "Rejected points (Gradient): " <> TextString[rejectedCounterGrad]]
			, "Rejected points (FuncValue): " <> TextString[rejectedCounterVal]]
			, "Rejected points (EVRatio): " <> TextString[rejectedCounterRat]]
			, "Added points: " <> TextString[Length[states]]];


			close[logger];
			Return[states];
		];


	doStep[pstate_,cstate_]:= (* [paststate, currentstate] *)
		Block[{nstates,dirs},
		
			dirs = determineDirections[cstate];
			dirs = processDirections[cstate, dirs*step];
			dirs = filterDirections[pstate, cstate, dirs];

			nstates = (cstate + #)& /@ dirs;

			Return[nstates];
		];


(* PRIVATE METHODS (informal) *)

	determineDirections[state_]:= (* [state, tolerance] *)
		Block[{nhess, directions},
			
			nhess = NHessian[func, state, Scale -> step/10];
			
			
			(* This should actually be checked in the "QValidDirection" method, but *)
			(* then the hessian would have to be recalculated.. so for performance reasons ... *)
			If[QEVRatioTooHigh[nhess],
				log[logger, "state rejected (evratiotoohigh) -" <> TextString[TextString[state]]];
				rejectedCounterRat += 1;
				
				Return[{}];
			];
			
			
			directions = Eigensystem[nhess, -numberldirs][[2]];

			Return[directions];
		];


	processDirections[state_,directions_] :=
		Block[{processed, i, nstate, p, f2, s},
			
			processed = {};
			
			For[i=1, i<=Length[directions], i++,
				AppendTo[processed, directions[[i]]];
				AppendTo[processed, -directions[[i]]];
			];


		(* if the surface is a minimum, we can apply FindMinimum to get a better approximation *)		
			If[minsurf,
			
				f2[p__?NumericQ] := func[{p}];
				For[i=1, i<=Length[processed], i++,
					
					nstate = state + processed[[i]];
					p = Table[Unique["p"], {Length[state]}];
				
					Quiet[s = FindMinimum[f2 @@ p, Thread[{p,nstate}]]];
					(* , MaxIterations->5 *)
					
					processed = ReplacePart[processed, i -> ((p /. s[[2]]) - state)];
				
				];
			];
			

			Return[processed];
			
		];


	filterDirections[pstate_,cstate_,directions_] := (* [paststate, currentstate, directions] *)
		Block[{filtered, i},

			filtered = {};

			For[i=1, i<=Length[directions], i++, (* for each direction *)

				If[QValidDirection[pstate, cstate, directions[[i]], directions],
					(* add *)
					AppendTo[filtered, directions[[i]]];
				];

			];

			Return[filtered];
		];


	norm[state_]:= (* [state] *)
		state/Norm[state]


	QValidDirection[pstate_,cstate_,dir_,dirs_]:= (* [paststate, currentstate, direction] *)
		Block[{nstate},
			nstate = cstate + dir;

				If[Not[QBack[pstate, nstate]],
					If[Not[QValueTooHigh[nstate]],
						If[Not[QGradientTooHigh[nstate,dirs]],
							If[Not[QNearStatesFull[nstate]],
								Return[True];
							,
								log[logger,
									"state rejected (nearstates) -" <>
									TextString[nstate] <> "-" <> TextString[pstate]];
							];
						,
							log[logger,
								"state rejected (gradienttoohigh) -" <>
								TextString[nstate] <> "-" <> TextString[pstate]];
							rejectedCounterGrad += 1;
						];
					,
						log[logger,
							"state rejected (valuetoohigh) -" <>
							TextString[func[nstate]] <> "-" <> TextString[nstate] <> "-" <> TextString[pstate]];
						rejectedCounterVal += 1;
					];
				,
					log[logger,
						"state rejected (back) -" <>
						TextString[nstate] <> "-" <> TextString[pstate]];
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


	QGradientTooHigh[state_,dirs_]:= (* [state] *)
		Block[{grad, proj},
			
			(* perform check only if gradtolfactor is finite *)
			If[gradtolfactor < \[Infinity],
				
				grad = NGradient[func, state];
	
				If[Norm[grad] < gradtolfactor,
					Return[False];
				,
					Return[True];
				];
				
			,
				Return[False];
			];

		];


	QValueTooHigh[nstate_]:=
		Block[{},
			
			(* perform check only if maxval is finite *)
			If[maxval < \[Infinity],
				
				If[Abs[func[nstate]] < maxval,
					Return[False];
				,
					Return[True];
				];
			
			,
				Return[False];
			];
		];


	(* Are we going back again? *)
	QBack[pstate_,nstate_]:= (* [paststate, currentstate, dir] *)
		Block[{},

			(* TODO: check if this makes sense in all poss. configs *)

			If[Norm[nstate-pstate] < step*0.7,
				Return[True];
			,(*else*)
				Return[False];
			];

		];


	QNearStatesFull[state_]:= (* [state] *)
		Block[{near},

			near = Nearest[states,state][[1]];

			If[Norm[state-near] < step*0.7,
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
