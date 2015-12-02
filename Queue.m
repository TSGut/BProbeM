(*
	Copyright 2015 Lukas Schneiderbauer


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



Needs["MathProg`Classes`","Classes.m"]; (* Class support *)

Class[Queue, Object,

	{
		list
	},{

	{new,(
		Module[{},
			new[super];
			list={};
		];
	)&},

	{push,( (* [elem] *)
		AppendTo[list,#1];
	)&},

	{pushList,( (* [list of elems] *)
		Module[{i, elist=#1},
			For[i=1,i<=Length[elist],i++,
				this.push[elist[[i]]];
			];
		];
	)&},

	{pop,(
		Module[{elem},
			elem = First[list];
			list = Drop[list,1];
			Return[elem];
		];
	)&},

	{size,(
		Length[list]
	)&},

	{getList, (
		Return[list];
	)&}

	}
];
