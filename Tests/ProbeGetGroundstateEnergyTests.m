ProbeInit[];

VerificationTest[ProbeGetGroundstateEnergy[{1, 0, 0}]
	, ProbeGetGroundstateEnergy[{1, 0, 0}]
	, TestID -> "ProbeGetGroundstateEnergy-inited"
];


ProbeInit[PauliMatrix[{1,2,3}], Probe->"Laplace"];

(* some naive tests *)
VerificationTest[ProbeGetGroundstateEnergy[{1, 0, 0}]
	, 2.
	, TestID -> "ProbeGetGroundstateEnergy-Laplace-1"
];
VerificationTest[ProbeGetGroundstateEnergy[{0, 1, 0}]
	, 2.
	, TestID -> "ProbeGetGroundstateEnergy-Laplace-2"
];
VerificationTest[ProbeGetGroundstateEnergy[{2, 0, 0}]
	, 3.
	, TestID -> "ProbeGetGroundstateEnergy-Laplace-3"
];
VerificationTest[ProbeGetGroundstateEnergy[{1, 1, 1}]
	, 2.5359
	, TestID -> "ProbeGetGroundstateEnergy-Laplace-4"
	, SameTest -> (Norm[#1-#2]<(10^-4)&)
];


ProbeInit[PauliMatrix[{1,2,3}], Probe->"DiracSq"];

VerificationTest[ProbeGetGroundstateEnergy[{1, 0, 0}]
	, 0.
	, TestID -> "ProbeGetGroundstateEnergy-DiracSq-1"
	, SameTest -> (Norm[#1-#2]<(10^-6)&)
];
VerificationTest[ProbeGetGroundstateEnergy[{0, 1, 0}]
	, 0.
	, TestID -> "ProbeGetGroundstateEnergy-DiracSq-2"
	, SameTest -> (Norm[#1-#2]<(10^-6)&)
];
VerificationTest[ProbeGetGroundstateEnergy[{2, 0, 0}]
	, 1.
	, TestID -> "ProbeGetGroundstateEnergy-DiracSq-3"
	, SameTest -> (Norm[#1-#2]<(10^-6)&)
];
VerificationTest[ProbeGetGroundstateEnergy[{1, 1, 1}]
	, 0.535898
	, TestID -> "ProbeGetGroundstateEnergy-DiracSq-4"
	, SameTest -> (Norm[#1-#2]<(10^-6)&)
];