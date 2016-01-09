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



BeginPackage["BProbe`Profiler`"];

	AddRecord::usage="";
	ShowProfileChart::usage="";
	ResetProfile::usage="";
	

Begin["`Private`"];

	(* initialize *)
	profiles = <||>;

	SetAttributes[AddRecord, HoldRest];
	AddRecord[id_, expr_, n_:1] := Block[{timing},
	
		If[n!=0,
			(* create id if doesnt exist *)
			If[!VectorQ[profiles[id]],
				AppendTo[profiles, id -> {}];
			];
			
			timing = AbsoluteTiming[expr];
			profiles[id] = profiles[id] ~Join~ Table[timing[[1]]/n,{n}];
			
			Return[timing[[2]]];
		,
			Return[expr];
		];
	
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