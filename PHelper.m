 (* ::Package:: *)

(*

	This file is part of BProbeM.
	
	"BProbeM, quantum and fuzzy geometry scanner" Copyright 2018 Timon Gutleb (timon.gutleb@gmail.com),
	see https://github.com/TSGut/BProbeM/
	
	Original version "BProbe" Copyright 2015 Lukas Schneiderbauer (lukas.schneiderbauer@gmail.com),
	see https://github.com/lschneiderbauer/BProbe

    BProbeM and BProbe are free software: you can redistribute them and/or modify
    them under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    BProbeM and BProbe are distributed in the hope that they will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BProbeM. If not, see <http://www.gnu.org/licenses/>.

*)


BeginPackage["BProbe`PHelper`"];

	PPMap::usage="";

Begin["`Private`"];

	PPMap[f_, expr_, progress_, update_] := Block[{ret, l, g, localprogress},
		
		progress = 0;
		l = Length[expr];
		
		SetSharedVariable[progress];
		DistributeDefinitions[update, l];
		ParallelEvaluate[localprogress=0]; localprogress=0;
		
		g = Function[x,
		
			ret = f[x];
		
			localprogress += 1;
			If[localprogress >= update,
				progress += N[localprogress]/l;
				localprogress = 0;
			];
			
			ret
		];
		
		ret = ParallelMap[g, expr];
		
		UnsetShared[progress];
		
		progress = 1.;
		Return[ret];
	];
	
	PPMap[f_, expr_] := ParallelMap[f, expr];
	
	SetAttributes[PPMap, HoldRest];
	

End[];
EndPackage[]; 
