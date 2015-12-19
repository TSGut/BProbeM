(* ::Package:: *)
 
BeginPackage["BProbe`Profiler`"];

	AddRecord::usage="";
	ShowProfileChart::usage="";
	ResetProfile::usage="";
	

Begin["`Private`"];

	(* initialize *)
	profiles = <||>;

	SetAttributes[AddRecord, HoldRest];
	AddRecord[id_, expr_] := Block[{timing},
	
		(* create id if doesnt exist *)
		If[!VectorQ[profiles[id]],
			AppendTo[profiles, id -> {}];
		];
		
		timing = Timing[expr];
		AppendTo[profiles[id],timing[[1]]];
		
		Return[timing[[2]]];
	
	];
	
	ShowProfileChart[] := Block[{bc},
		bc = BarChart[
			(Tooltip[
				GetTotalTime[#],
				GetText[#]
			])& /@ Keys[profiles],
			ChartLegends -> Keys[profiles],
			ChartStyle -> "DarkRainbow",
			PlotTheme -> "Business"
		];

		Return[bc];	
	];
	
	GetText[id_] := Block[{text},
		text = "_" <> TextString[id] <> "_\n"
			<> "Mean:\t(" <> TextString[GetMean[id]] <>  " \[PlusMinus] "
			<> TextString[GetStandardDeviation[id]] <> ") s\n"
			<> "\[NumberSign] calls:\t" <> TextString[GetNumberOfCalls[id]];
		Return[text];
	];

	GetMean[id_] := Mean[profiles[id]];
	GetStandardDeviation[id_] := StandardDeviation[profiles[id]];
	GetNumberOfCalls[id_] := Length[profiles[id]];
	GetTotalTime[id_] := Total[profiles[id]];
	
	ResetProfile[] := Block[{},
		profiles = <||>;
	];

End[];
EndPackage[];