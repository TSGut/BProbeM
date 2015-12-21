ProbeInit[];

VerificationTest[ProbeGetMinEigenvalue[{1, 0, 0}]
	, ProbeGetMinEigenvalue[{1, 0, 0}]
	, TestID -> "ProbeGetMinEigenvalue-inited"
];


ProbeInit[PauliMatrix[{1,2,3}], Probe->"Laplace"];

(* some naive tests *)
VerificationTest[ProbeGetMinEigenvalue[{1, 0, 0}]
	, 2.
	, TestID -> "ProbeGetMinEigenvalue-Laplace-1"
];
VerificationTest[ProbeGetMinEigenvalue[{0, 1, 0}]
	, 2.
	, TestID -> "ProbeGetMinEigenvalue-Laplace-2"
];
VerificationTest[ProbeGetMinEigenvalue[{2, 0, 0}]
	, 3.
	, TestID -> "ProbeGetMinEigenvalue-Laplace-3"
];
VerificationTest[ProbeGetMinEigenvalue[{1, 1, 1}]
	, 2.5359
	, TestID -> "ProbeGetMinEigenvalue-Laplace-4"
	, SameTest -> (Norm[#1-#2]<(10^-4)&)
];


ProbeInit[PauliMatrix[{1,2,3}], Probe->"DiracSq"];

VerificationTest[ProbeGetMinEigenvalue[{1, 0, 0}]
	, 0.
	, TestID -> "ProbeGetMinEigenvalue-DiracSq-1"
	, SameTest -> (Norm[#1-#2]<(10^-6)&)
];
VerificationTest[ProbeGetMinEigenvalue[{0, 1, 0}]
	, 0.
	, TestID -> "ProbeGetMinEigenvalue-DiracSq-2"
	, SameTest -> (Norm[#1-#2]<(10^-6)&)
];
VerificationTest[ProbeGetMinEigenvalue[{2, 0, 0}]
	, 1.
	, TestID -> "ProbeGetMinEigenvalue-DiracSq-3"
	, SameTest -> (Norm[#1-#2]<(10^-6)&)
];
VerificationTest[ProbeGetMinEigenvalue[{1, 1, 1}]
	, 0.535898
	, TestID -> "ProbeGetMinEigenvalue-DiracSq-4"
	, SameTest -> (Norm[#1-#2]<(10^-6)&)
];