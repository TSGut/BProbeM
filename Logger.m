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



Needs["MathProg`Classes`","Classes.m"] (* Class support *)

Class[Logger, Object,

(* LOCAL VARIABLES *)
	{
		stream
	},{

(* PUBLIC METHODS *)

	{new,( (* [filename] *)
		Block[{filename=#1},
			new[super];

			If[filename != "",
				Quiet[Close[filename]];
				stream = OpenWrite[filename];
			,
				stream = OpenWrite[];
			];
			
		];
		
	)&},

	{log,( (* [string] *)
		Block[{str=#1},
			WriteString[stream,str<>"\n"];
		];
	)&},
	
	{close,(
		Close[stream];
	)&}

	}
];