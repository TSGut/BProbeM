ProbeInit[];

VerificationTest[ProbeReset[]
	, ProbeReset[]
	, TestID -> "ProbeReset-inited"
];



ProbeInit[PauliMatrix[{1,2,3}], StartingPoint->{1,0,0}];

(* a naive tests *)
VerificationTest[Block[{},
		ProbeReset[StartingPoint->{1,0,1}];
		ProbeGetPointList[]
	]
	, {{1,0,1}}
	, TestID -> "ProbeReset-1"
]