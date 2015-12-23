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
	ProbeGetPointList::usage = "";
	ProbeReset::usage = "";
	ProbeGetMinEigenvalue::usage = "";
	ProbeGetEigenvalues::usage = "";
	ProbeGetState::usage = "";
	MatrixRepSU2::usage = "";
	MatrixRepSU3::usage = "";
	RunTests::usage = "";
(***************************************************************************************)
(* for usage messages: see the end of this package *)


Begin["`Private`"];
	Get[ "BProbe`Scan`" ];
	Get[ "BProbe`SU2Gen`" ];
	Get[ "BProbe`SU3Gen`" ];
	

	Options[ProbeInit] = Options[BProbe`Scan`init];
	ProbeInit[] := Block[{}, inited=False; ];
	ProbeInit[t_?(VectorQ[#,MatrixQ]&), opts:OptionsPattern[]] := Block[{info},
		
		info = BProbe`Scan`init[t, FilterRules[{opts}, Options[BProbe`Scan`init]]];
		
		inited=True; (* say: okay, we did a initialization *)
		
		Print[Panel[TextGrid[
			info,
			Dividers -> Center,
			Alignment -> {{Right,Center},Table[Top,Length[info]]},
			Spacings -> {3,2},
			ItemSize -> {{Automatic, Automatic}}
		]]];
	];
	
	Options[ProbeReset] = Options[BProbe`Scan`reset];
	ProbeReset[opts:OptionsPattern[]] := Block[{},
		
		BProbe`Scan`reset[opts];
		
	] /; inited;

	Options[ProbeScan] = Options[BProbe`Scan`start] ~Join~ {UpdateInterval->0.1};
	ProbeScan[stepsize_?NumericQ /; stepsize > 0, opts:OptionsPattern[]] := Block[{},

		PrintTemporary["Scanning surface ... ",ProgressIndicator[Appearance -> "Necklace"]];
		
		Monitor[
			BProbe`Scan`start[stepsize,
				FilterRules[{opts}, Options[BProbe`Scan`start]]
			];
		,
			(* status message *)
			Refresh[ generateStatus[FilterRules[{opts}, Options[BProbe`Scan`start]]], TrackedSymbols->{}, UpdateInterval -> OptionValue[UpdateInterval]]
		];

		(* print it out again, so it doesnt just vanish when finished *)
		Print[generateStatus[FilterRules[{opts}, Options[BProbe`Scan`start]]]];
		
		(* print profiling chart if enabled *)
		If[OptionValue[Profiling], Print[BProbe`Profiler`ShowProfileChart[]]];
		
	] /; inited;
	
	ProbeGetPointList[] := Block[{},
		Return[BProbe`Scan`getList[]];
	] /; inited;
	
	ProbeGetMinEigenvalue[p_?(VectorQ[#,NumericQ]&)] := Block[{},
		Return[BProbe`Scan`getMinEigenvalue[p]];
	] /; inited;
	
	ProbeGetEigenvalues[p_?(VectorQ[#,NumericQ]&)] := Block[{},
		Return[BProbe`Scan`getEigenvalues[p]];
	] /; inited;
	
	ProbeGetState[p_?(VectorQ[#,NumericQ]&)] := Block[{},
		Return[BProbe`Scan`getState[p]];
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
	RunTests[] := Block[{testdir, filenames, reports, report, i, allsucceeded, testcount, output, ptemp},
		
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

		
		For[i=1,i<=Length[reports],i++,
		
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
		
		];
		
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

	(* status message: accesses private variables of `Scan` *)
	generateStatus[options:OptionsPattern[]] := Block[{status},
		opts[symbol_] := OptionValue[BProbe`Scan`start, options, symbol];
	
		status = {
			{ "Number of Total Points Gathered" , Style[TextString[Length[BProbe`Scan`Private`pointlist]],Bold] },
			{ "Number of Points in Queue To Process" , BProbe`Scan`Private`size[BProbe`Scan`Private`boundary] },
			{ "Maximal Occured Eigenvalue-Ratio" , BProbe`Scan`Private`maxEVRatioTracker }
		};
		
		If[opts[GradientTracker] || (opts[MaxGradient] < \[Infinity]),
			AppendTo[status, { "Maximal Occured Norm of Gradient" , BProbe`Scan`Private`maxGradientTracker }];
		];
		
		If[opts[EnergyTracker] || (opts[MaxEnergy] < \[Infinity]),
			AppendTo[status, { "Maximal Occured Displacement Energy" , BProbe`Scan`Private`maxFuncValTracker }];
		];
		
		If[opts[MaxEVRatio] < \[Infinity],
			AppendTo[status, { "Rejected pts (EVRatio)" , BProbe`Scan`Private`rejectedCounterRat }];
		];
		
		If[opts[MaxEnergy] < \[Infinity],
			AppendTo[status, { "Rejected pts (FuncValue)" , BProbe`Scan`Private`rejectedCounterVal }];
		];
		
		If[opts[MaxGradient] < \[Infinity],
			AppendTo[status, { "Rejected pts (Gradient)" , BProbe`Scan`Private`rejectedCounterGrad }];
		];
		
		Panel[TextGrid[
			status,
			Dividers -> Center,
			Alignment -> {{Right,Center},Table[Top,Length[status]]},
			Spacings -> {3,2},
			ItemSize -> {{Automatic, Fit}}
		], "Status Information", ImageSize->Full]
	];
	
	

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
	
	ProbeScan::usage = BProbe`Private`header["ProbeScan", {{"Integer", "dimension"}, {"Real", "step size"}}] <> " performs the actual scanning procedure. It implements an algorithm to rasterize the semi-classical limit of the brane configuration defined by a set of matrices as submanifold of the target space.";
	
	ProbeGetPointList::usage = BProbe`Private`header["ProbeGetPointList",{}] <> " returns a " <> BProbe`Private`doc["List"] <> " of already calculated points. A point is itself represented as a " <> BProbe`Private`doc["List"] <> " consisting of d " <> BProbe`Private`doc["Real"] <> "s.";
	
	ProbeReset::usage = BProbe`Private`header["ProbeReset",{}] <> " resets the package in a way, so that the command " <> BProbe`Private`header["ProbeScan", {{"Integer", "dimension"}, {"Real", "step size"}}] <> " starts a completely new calculation.";
	
	ProbeGetMinEigenvalue::usage = BProbe`Private`header["ProbeGetMinEigenvalue", {{"List", "point"}}] <> " returns the minimal eigenvalue (with respect to the modulus) of the (Laplace-/Dirac-) operator in question for a given point.";
	
	ProbeGetEigenvalues::usage = BProbe`Private`header["ProbeGetEigenvalues", {{"List", "point"}}] <> " returns the eigenvalues of the (Laplace-/Dirac-) operator in question for a given point.";
	
	ProbeGetState::usage = BProbe`Private`header["ProbeGetState", {{"List", "point"}}] <> " returns the eigenvector corresponding to the minimal eigenvalue (with respect to the modulus) of the (Laplace-/Dirac-) operator in question for a given point.";
	
	MatrixRepSU2::usage = BProbe`Private`header["MatrixRepSU2", {{"Integer", "dimension"}}] <> " returns a " <> BProbe`Private`doc["List"] <> " of three matrices, constituting a specific matrix representation of SU(2) acting on a representation space of the dimension given as argument."
	
	MatrixRepSU3::usage = BProbe`Private`header["MatrixRepSU3", {{"List", "highest_weight"}}] <> " returns a " <> BProbe`Private`doc["List"] <> " of eight matrices, constituting a specific matrix representation of SU(3) with highest weight given as argument."
	
	RunTests::usage = "Run the Test-Suite.";
(***************************************************************************************)

EndPackage[];

Print["Loaded BProbe. See the " <> BProbe`Private`link["index", "documentation"] <> " for help."];
