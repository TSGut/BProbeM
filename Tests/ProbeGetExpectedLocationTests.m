ProbeInit[];

VerificationTest[ProbeGetExpectedLocation[{1, 0, 0}]
	, ProbeGetExpectedLocation[{1, 0, 0}]
	, TestID -> "ProbeGetExpectedLocation-inited"
];


ProbeInit[PauliMatrix[{1,2,3}], Probe->"Laplace"];

(* some naive tests *)
VerificationTest[ProbeGetExpectedLocation[{2, 0, 0}]
	, {1., 0., 0.}
	, TestID -> "ProbeGetExpectedLocation-Laplace-1"
	, SameTest -> (Norm[#1-#2]<(10^-6)&)
];
VerificationTest[ProbeGetExpectedLocation[{0, 2, 0}]
	, {0., 1., 0.}
	, TestID -> "ProbeGetExpectedLocation-Laplace-2"
	, SameTest -> (Norm[#1-#2]<(10^-6)&)
];
VerificationTest[ProbeGetExpectedLocation[{3, 0, 0}]
	, {1., 0., 0.}
	, TestID -> "ProbeGetExpectedLocation-Laplace-3"
	, SameTest -> (Norm[#1-#2]<(10^-6)&)
];
VerificationTest[ProbeGetExpectedLocation[{1, 1, 1}]
	, {0.57735, 0.57735, 0.57735}
	, TestID -> "ProbeGetExpectedLocation-Laplace-4"
	, SameTest -> (Norm[#1-#2]<(10^-4)&)
];

VerificationTest[ProbeGetExpectedLocation[{1, 0}]
	, {0., 0., 1.}
	, TestID -> "ProbeGetExpectedLocation-Laplace-5"
	, SameTest -> (Norm[#1-#2]<(10^-4)&)
];
VerificationTest[ProbeGetExpectedLocation[{1, I}]
	, {0., 2., 0.}
	, TestID -> "ProbeGetExpectedLocation-Laplace-6"
	, SameTest -> (Norm[#1-#2]<(10^-4)&)
];


ProbeInit[PauliMatrix[{1,2,3}], Probe->"DiracSq"];

VerificationTest[ProbeGetExpectedLocation[{2, 0, 0}]
	, {1., 0., 0.}
	, TestID -> "ProbeGetExpectedLocation-DiracSq-1"
	, SameTest -> (Norm[#1-#2]<(10^-6)&)
];
VerificationTest[ProbeGetExpectedLocation[{0, 2, 0}]
	, {0., 1., 0.}
	, TestID -> "ProbeGetExpectedLocation-DiracSq-2"
	, SameTest -> (Norm[#1-#2]<(10^-6)&)
];
VerificationTest[ProbeGetExpectedLocation[{3, 0, 0}]
	, {1., 0., 0.}
	, TestID -> "ProbeGetExpectedLocation-DiracSq-3"
	, SameTest -> (Norm[#1-#2]<(10^-6)&)
];
VerificationTest[ProbeGetExpectedLocation[{1, 1, 1}]
	, {0.57735, 0.57735, 0.57735}
	, TestID -> "ProbeGetExpectedLocation-DiracSq-4"
	, SameTest -> (Norm[#1-#2]<(10^-4)&)
];

VerificationTest[ProbeGetExpectedLocation[{1, 0, 0, 0}]
	, {0., 0., 1.}
	, TestID -> "ProbeGetExpectedLocation-DiracSq-5"
	, SameTest -> (Norm[#1-#2]<(10^-4)&)
];
VerificationTest[ProbeGetExpectedLocation[{1, I, 0, I}]
	, {0., 2., -1.}
	, TestID -> "ProbeGetExpectedLocation-DiracSq-6"
	, SameTest -> (Norm[#1-#2]<(10^-4)&)
];