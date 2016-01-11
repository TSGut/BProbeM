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
(* see the documentation for explanations *)
(***************************************************************************************)
	ProbeInit::usage = "";
	ProbeScan::usage = "";
	ProbeGetPoints::usage = "";
	ProbeReset::usage = "";
	ProbeGetGroundstateEnergy::usage = "";
	ProbeGetEnergies::usage = "";
	ProbeGetGroundstate::usage = "";
	ProbeGetExpectedLocation::usage = "";
	ProbeGetGroundstates::usage = "";
	MatrixRepSU2::usage = "";
	MatrixRepSU3::usage = "";
	RunTests::usage = "";
(***************************************************************************************)
(* for usage messages: see the end of this package *)


Begin["`Private`"];
	Get[ "BProbe`Scan`" ];
	Get[ "BProbe`SU2Gen`" ];
	Get[ "BProbe`SU3Gen`" ];
	Get[ "BProbe`PHelper`" ];
	
	
	Options[ProbeInit] = Options[BProbe`Scan`init];
	ProbeInit[] := Block[{}, inited=False; ];
	ProbeInit[t_?(VectorQ[#,MatrixQ]&), opts:OptionsPattern[]] := Block[{},
		
		Info = BProbe`Scan`init[t, FilterRules[{opts}, Options[BProbe`Scan`init]]];
		inited=True; (* say: okay, we did a initialization *)
		
		Print[styleInitInfo[Info]];
	];
	
	Options[ProbeReset] = Options[BProbe`Scan`reset];
	ProbeReset[opts:OptionsPattern[]] := Block[{info},
		
		info = BProbe`Scan`reset[opts];
		
		(* print new status info only if StartingPoint has changed *)
		If[info["StartingPoint"] != Info["StartingPoint"],
			Print[styleInitInfo[info]];
		];
		
		Info = info;
		
	] /; inited;

	Options[ProbeScan] = Options[BProbe`Scan`start] ~Join~ {UpdateInterval->0.1};
	ProbeScan[opts:OptionsPattern[]] := Block[{},
		
		PrintTemporary["Scanning surface ... ",ProgressIndicator[Appearance -> "Necklace"]];
		
		Monitor[
			BProbe`Scan`start[ FilterRules[{opts}, Options[BProbe`Scan`start]] ];
		,
			(* status message *)
			Refresh[ generateStatus[FilterRules[{opts}, Options[BProbe`Scan`start]]], TrackedSymbols->{}, UpdateInterval -> OptionValue[UpdateInterval]]
		];
		
		(* print it out again, so it doesnt just vanish when finished *)
		If[Length[BProbe`Scan`getList[]] > 1,
			Print[generateStatus[FilterRules[{opts}, Options[BProbe`Scan`start]]]];
		,
			(* else print some recommendation to get something work *)
			Print["Oops, no points have been collected, ..."];
			If[OptionValue[Dimension] == 0,
				Print["... because the specified dimension is zero. You might want to override it via the 'Dimension'-option."];
			,
				If[BProbe`Scan`Private`rejectedCounterGrad > 0,
					Print["... you might want to raise the gradient cutoff via the 'MaxGradient' option."];
				];
				If[BProbe`Scan`Private`rejectedCounterRat > 0,
					Print["... you might want to raise the eigenvalue cutoff via the 'MaxEV' option."];
				];
				If[BProbe`Scan`Private`rejectedCounterVal > 0,
					Print["... you might want to raise the energy cutoff via the 'MaxEnergy' option."];
				];
				If[BProbe`Scan`Private`rejectedCounterNNS > 0 &&
					BProbe`Scan`Private`rejectedCounterGrad == 0 &&
					BProbe`Scan`Private`rejectedCounterRat == 0 &&
					BProbe`Scan`Private`rejectedCounterVal == 0 &&
					OptionValue[ReplacePoints],
					Print["... you might want to raise the step size via the 'StepSize' option or deactivate the 'ReplacePoints' option."];
				];
			];
			
			ProbeReset[];
		];
		
		(* print profiling chart if enabled *)
		If[OptionValue[Profiling], Print[BProbe`Profiler`ShowProfileChart[]]];
		
	] /; inited;
	
	ProbeGetPoints[] := Block[{},
		Return[BProbe`Scan`getList[]];
	] /; inited;
	
	ProbeGetGroundstateEnergy[p_?(VectorQ[#,NumericQ]&)] := Block[{},
		Return[BProbe`Scan`getMinEigenvalue[p]];
	] /; inited;
	
	ProbeGetEnergies[p_?(VectorQ[#,NumericQ]&)] := Block[{},
		Return[BProbe`Scan`getEigenvalues[p]];
	] /; inited;
	
	ProbeGetGroundstate[p_?(VectorQ[#,NumericQ]&)] := Block[{},
		Return[BProbe`Scan`getState[p]];
	] /; inited;
	
	ProbeGetGroundstates[] := Block[{progress},
		If[!IntegerQ[plHash] || Hash[BProbe`Scan`getList[]] != plHash,
			PrintTemporary["* Generate ground states ... ", ProgressIndicator[Dynamic[progress]]];
			
			Quiet[LaunchKernels[]];
			DistributeDefinitions[BProbe`Scan`getState];
			
			states = PPMap[(
				BProbe`Scan`getState[#]
			)&, BProbe`Scan`getList[], progress, 100];
			
			plHash = Hash[BProbe`Scan`getList[]];
		];
		
		Return[states];
	] /; inited;
	
	ProbeGetExpectedLocation[state_?(VectorQ[#,NumericQ]&) /;
		Length[state]==Info["HilbertSpaceDimension"]
	] := Block[{},
		Return[BProbe`Scan`getExpectedLocation[state]];
	] /; inited;
	
	ProbeGetExpectedLocation[p_?(VectorQ[#,NumericQ]&) /;
		Length[p]==Info["TargetSpaceDimension"]
	] := Block[{},
		Return[BProbe`Scan`getExpectedLocation[BProbe`Scan`getState[p]]];
	] /; inited;
	
	MatrixRepSU2[n_?NumericQ /; n>0] := Block[{},
		Return[BProbe`SU2Gen`Private`MatrixRepSU2[n]];
	];
	
	MatrixRepSU3[list_?(VectorQ[#,NumericQ] &) /;
			Length[list]==2 &&
			list[[1]]>=0 &&
			list[[2]]>=0
	] := Block[{},
		Return[BProbe`SU3Gen`Private`MatrixRepSU3[list]];
	];
	
	
	
	
	(* runs all Tests specified in ./Tests/ *)
	(****************************************)
	RunTests[] := Block[{testdir, filenames, reports, report, allsucceeded, testcount},
		
		testcount = 0;
		allsucceeded=True;
		testdir = FileNameJoin[{thisDirectory[], "Tests"}];
		filenames = FileNames["*Tests.m", {testdir}];
		
		
		PrintTemporary["Running Tests ",ProgressIndicator[Appearance -> "Ellipsis"]];
		
		Block[{PrintTemporary,Print},
			(* deactivate outputs *)
			Unprotect[PrintTemporary]; PrintTemporary = Null &;
			Unprotect[Print]; Print = Null &;
			
			(* run tests *)
			reports = TestReport[#]& /@ filenames;
		
		]; (* Print and PrintTemporary are usable again at this point *)
		
		
		Do[
			testcount += Length[reports[[i]]["TestResults"]];
			
			If[reports[[i]]["AllTestsSucceeded"]==False,
				allsucceeded=False;
				
				report = {
					{ "Tests failed", Style[reports[[i]]["TestsFailedCount"],{Bold,Red}] },
					{ "Tests succeeded", Style[reports[[i]]["TestsSucceededCount"],{Bold,Darker[Green]}] },
					{ "Time elapsed", reports[[i]]["TimeElapsed"] },
					{ "Failed tests", Union[Values[reports[[i]]["TestsFailed"]]] }
				};
				
				Print[Panel[TextGrid[
					report,
					Dividers -> Center,
					Alignment -> {{Left,Center}},
					Spacings -> {3,2}
				], FileBaseName[filenames[[i]]] ]];
			
			];
		, {i, Length[reports]}];
		
		If[!allsucceeded,
			Print["!!! Warning: Some tests failed !!!"];
			Return[reports];
		,
			Print["All " <> TextString[testcount] <> " tests succeeded."];
		];
		
	];
	
	(* Mathematica seems to have the weirdest directory-handling I've ever seen, *)
	(* so I need this ugly workaround *)
	thisDirectory[] = DirectoryName[$InputFileName];
	
	styleInitInfo[info_] := Block[{textgrid, hevs},
		
		hevs = info["HEigenvaluesSP"];
		
		Do[
			hevs[[i]] = If[i <= info["BraneDimension"],
				Style[TextString[hevs[[i]]], Darker[Green]]
			,
				Style[TextString[hevs[[i]]], Darker[Red]]
			];
		, {i, Length[hevs]}];
		
		textgrid = {
			{ "Energy Probe", Style[info["EnergyProbe"], Bold] },
			{ "Starting Point (SP)", MatrixForm[info["StartingPoint"]] },
			{ "Energy at SP" , TextString[info["EnergySP"]] },
			{ "Norm of Gradient at SP" , TextString[info["GradientSP"]] },
			{ "Absolute Hessian Eigenvalues at SP", MatrixForm[hevs] },
			{ "Local brane dimension at SP", Style[TextString[info["BraneDimension"]],{Darker[If[info["BraneDimension"]==0,Red,Green]],Bold}] },
			{ "Dimension of Target Space", Style[TextString[info["TargetSpaceDimension"]],Bold] },
			{ "Dimension of Hilbert Space", Style[TextString[info["HilbertSpaceDimension"]],Bold] },
			{ "Step size guess", TextString[info["StepSize"]] }
		};
		textgrid = Transpose[{ Style[#,GrayLevel[.25]]& /@ Transpose[textgrid][[1]], Transpose[textgrid][[2]] }];
		
		Return[Panel[TextGrid[
			textgrid,
			Dividers -> Center,
			Alignment -> {{Right,Center},Table[Top,Length[info]]},
			Spacings -> {3,2},
			ItemSize -> {{Automatic, Automatic}}
		]]];
	];
	
	(* status message: accesses private variables of `Scan` *)
	generateStatus[options:OptionsPattern[]] := Block[{status, points, tracker, rejections},
		opts[symbol_] := OptionValue[BProbe`Scan`start, options, symbol];
		
		points = {
			{ "Number of total points gathered" , Style[TextString[Length[BProbe`Scan`Private`pointlist]],Bold] },
			{ "Number of points currently processing" , Length[BProbe`Scan`Private`boundary] }
		};
		points = Transpose[{ Style[#,GrayLevel[.25]]& /@ Transpose[points][[1]], Transpose[points][[2]] }];
		
		tracker = Flatten[Reap[
			If[opts[GradientTracker] || (opts[MaxGradient] < \[Infinity]),
				Sow[{ "Largest emerged Gradient norm" , TextString[BProbe`Scan`Private`maxGradientTracker] }];
			];
			
			If[opts[EnergyTracker] || (opts[MaxEnergy] < \[Infinity]),
				Sow[{ "Smallest/Largest emerged Displacement Energy" , StringForm["`4` `1` `3` `2` `5`",TextString[#1],TextString[#2],Style[",",Gray,Larger,Bold],Style["[",Gray,Larger,Bold],Style["]",Gray,Larger,Bold]]& @@ BProbe`Scan`Private`intEnergyTracker }];
			];
			
			If[opts[EVTracker] || (opts[MaxEV] < \[Infinity]),
				Sow[{ "Largest emerged 'small' Eigenvalue" , TextString[BProbe`Scan`Private`maxEVTracker] }];
			];
		][[2]],1];
		If[Length[tracker]!=0,
			tracker = Transpose[{ Style[#,GrayLevel[.25]]& /@ Transpose[tracker][[1]], Transpose[tracker][[2]] }];
		];
		
		rejections = Flatten[Reap[
			If[opts[MaxEV] < \[Infinity],
				Sow[{ "Rejected points due to 'MaxEV'" , Style[TextString[BProbe`Scan`Private`rejectedCounterRat], Darker[Red]] }];
			];
			
			If[opts[MaxEnergy] < \[Infinity],
				Sow[{ "Rejected points due to 'MaxEnergy'" , Style[TextString[BProbe`Scan`Private`rejectedCounterVal], Darker[Red]] }];
			];
			
			If[opts[MaxGradient] < \[Infinity],
				Sow[{ "Rejected points due to 'MaxGradient'" , Style[TextString[BProbe`Scan`Private`rejectedCounterGrad], Darker[Red]] }];
			];
		][[2]],1];
		If[Length[rejections]!=0,
			rejections = Transpose[{ Style[#,GrayLevel[.25]]& /@ Transpose[rejections][[1]], Transpose[rejections][[2]] }];
		];
		
		status = Flatten[Reap[
			Sow[{ getPanel["Status Information", points] }];
			If[Length[tracker] > 0, Sow[{ getPanel["Tracker", tracker] }]];
			If[Length[rejections] > 0, Sow[{ getPanel["Rejection of Points", rejections] }]];
		][[2]],1];
		
		Grid[status, Spacings -> {0,1}]
	];
	
	getPanel[title_, textgrid_] :=
		Panel[TextGrid[
			textgrid,
			Dividers -> Center,
			Alignment -> {{Right,Center},Table[Top,Length[textgrid]]},
			Spacings -> {3,2},
			ItemSize -> {{Automatic, Fit}}
		], title, ImageSize->Full];
	
	
	(* formatting stuff for usage messages	*)
	(****************************************)
	
	link[name_] := ToString[Hyperlink[name, "file://" <> FileNameJoin[{$UserBaseDirectory, "Applications", "BProbe", "Documentation", name <> ".html"}]], StandardForm];
	link[name_, alias_] := ToString[Hyperlink[alias, "file://" <> FileNameJoin[{$UserBaseDirectory, "Applications", "BProbe", "Documentation", name <> ".html"}]], StandardForm];
	doc[name_] := ToString[Hyperlink[name, "paclet:ref/" <> name], StandardForm];
	italic[name_] := ToString[Style[name, Italic, Small], StandardForm];
	bold[name_] := ToString[Style[name, Bold], StandardForm];
	header[name_, args_] := Block[{i, str},
		str = link[name] <> "[";
		For[i=1,i<=Length[args],i++,
			str = str <> " _" <> doc[args[[i,1]]] <> " :: " <> italic[args[[i,2]]] <> " ";
			If[i<Length[args], str = str <> ","];
		];

		str = str <> "]";
		Return[str];
    ];
	(*										*)
	(****************************************)


End[];



(* The sole PUBlIC API of this package *)
(***************************************************************************************)
	ProbeInit::usage = BProbe`Private`header["ProbeInit", {{"List", "list of matrices"}}] <> " expects a list of d matrices. This method takes care of building the appropriate (Laplace-/Dirac-) operator for you which is needed to perform the rasterizing procedure. " <> BProbe`Private`bold["Note that calling this method is absolutely required before other methods of this package can be used!"] <> "."
	
	ProbeScan::usage = BProbe`Private`header["ProbeScan", {{"Real", "step size"}}] <> " performs the actual scanning procedure. It implements an algorithm to rasterize the semi-classical limit of the brane configuration defined by a set of matrices as submanifold of the target space.";
	
	ProbeGetPoints::usage = BProbe`Private`header["ProbeGetPoints",{}] <> " returns a " <> BProbe`Private`doc["List"] <> " of already calculated points. A point is itself represented as a " <> BProbe`Private`doc["List"] <> " consisting of d " <> BProbe`Private`doc["Real"] <> "s.";
	
	ProbeReset::usage = BProbe`Private`header["ProbeReset",{}] <> " resets the package in a way, so that the command " <> BProbe`Private`header["ProbeScan", {{"Integer", "dimension"}, {"Real", "step size"}}] <> " starts a completely new calculation.";
	
	ProbeGetGroundstateEnergy::usage = BProbe`Private`header["ProbeGetGroundstateEnergy", {{"List", "point"}}] <> " returns the ground state energy of the (Laplace-/Dirac-) operator in question for a given point.";
	
	ProbeGetEnergies::usage = BProbe`Private`header["ProbeGetEnergies", {{"List", "point"}}] <> " returns the energies of the (Laplace-/Dirac-) operator in question for a given point.";
	
	ProbeGetGroundstate::usage = BProbe`Private`header["ProbeGetGroundstate", {{"List", "point"}}] <> " returns the ground state of the (Laplace-/Dirac-) operator in question for a given point.";
	
	ProbeGetGroundstates::usage = BProbe`Private`header["ProbeGetGroundstates", {}] <> " returns the ground states of the (Laplace-/Dirac-) operator for all points given by " <>  BProbe`Private`header["ProbeGetPoints", {}] <> "."
	
	ProbeGetExpectedLocation::usage = BProbe`Private`header["ProbeGetExpectedLocation", {{"List", "point"}}] <> " returns the expectation value of the quantized embedding functions corresponding to the ground state of the (Laplace-/Dirac-) operator in question for a given point.\n" <>
	BProbe`Private`header["ProbeGetExpectedLocation", {{"List", "state"}}] <> " returns the expectation value of the quantized embedding functions for the given state.";
	
	MatrixRepSU2::usage = BProbe`Private`header["MatrixRepSU2", {{"Integer", "dimension"}}] <> " returns a " <> BProbe`Private`doc["List"] <> " of three matrices, constituting a specific matrix representation of SU(2) acting on a representation space of the dimension given as argument."
	
	MatrixRepSU3::usage = BProbe`Private`header["MatrixRepSU3", {{"List", "highest_weight"}}] <> " returns a " <> BProbe`Private`doc["List"] <> " of eight matrices, constituting a specific matrix representation of SU(3) with highest weight given as argument."
	
	RunTests::usage = "Run the Test-Suite.";
(***************************************************************************************)

EndPackage[];

Print["Loaded BProbe. See the " <> BProbe`Private`link["index", "documentation"] <> " for help."];