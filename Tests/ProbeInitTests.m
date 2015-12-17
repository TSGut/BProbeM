(* ::Package:: *)

VerificationTest[
	Block[{t},
	
		t = PauliMatrix[{1,2,3}];
	
		ProbeInit[t];
	]
	, Null
	, TestID -> "ProbeInit-wo-opts"
];
VerificationTest[
Block[{t},

		t = PauliMatrix[{1,2,3}];

		ProbeInit[t, Probe->"Laplace"];
	]
	, Null
	, TestID -> "ProbeInit-Laplace"
];
VerificationTest[
	Block[{t},

		t = PauliMatrix[{1,2,3}];

		ProbeInit[t, Probe->"Dirac"];
	]
	, Null
	, TestID -> "ProbeInit-Dirac"
];
VerificationTest[
	Block[{t},

		t = PauliMatrix[{1,2,3}];

		ProbeInit[t, Probe->"DiracSq"];
	]
	, Null
	, TestID -> "ProbeInit-DiracSq"
];
VerificationTest[
	Block[{t},

		t = PauliMatrix[{1,2,3}];

		ProbeInit[t, StartingPoint->{1,0,0}];
	]
	, Null
	, TestID -> "ProbeInit-StartingPoint"
];
VerificationTest[
Block[{t},

		t = PauliMatrix[{1,2,3}];

		ProbeInit[t, Subspace->{1,2}];
	]
	, Null
	, TestID -> "ProbeInit-Subspace"
];


(* set up *)
ProbeInit[PauliMatrix[{1,2,3}],StartingPoint->{1,0,0}];

VerificationTest[ProbeGetPointList[]
	, {{1,0,0}}
	, TestID -> "ProbeGetPointList-startingpoint"
]