ProbeInit[];

VerificationTest[ProbeGetEnergies[{1, 0, 0}]
	, ProbeGetEnergies[{1, 0, 0}]
	, TestID -> "ProbeGetEnergies-inited"
];


ProbeInit[PauliMatrix[{1,2,3}], Probe->"Laplace"];

VerificationTest[ProbeGetEnergies[{1, 0, 0}]
	, {6., 2.}
	, TestID -> "ProbeGetEnergies-Laplace-1"
];
VerificationTest[ProbeGetEnergies[{0, 0, 1}]
	, {6., 2.}
	, TestID -> "ProbeGetEnergies-Laplace-2"
];
VerificationTest[ProbeGetEnergies[{2, 0, 0}]
	, {11., 3.}
	, TestID -> "ProbeGetEnergies-Laplace-3"
];
VerificationTest[ProbeGetEnergies[{1, 1, 1}]
	, {9.4641, 2.5359}
	, TestID -> "ProbeGetEnergies-Laplace-4"
	, SameTest -> (Norm[#1-#2]<(10^-4)&)
];

ProbeInit[PauliMatrix[{1,2,3}], Probe->"DiracSq"];

VerificationTest[ProbeGetEnergies[{1, 0, 0}]
	, {10.4721, 4., 1.52786, -1.51909*10^-16}
	, TestID -> "ProbeGetEnergies-DiracSq-1"
	, SameTest -> (Norm[#1-#2]<(10^-4)&)
];
VerificationTest[ProbeGetEnergies[{0, 0, 1}]
	, {10.4721, 4., 1.52786, 0.}
	, TestID -> "ProbeGetEnergies-DiracSq-2"
	, SameTest -> (Norm[#1-#2]<(10^-4)&)
];
VerificationTest[ProbeGetEnergies[{2, 0, 0}]
	, {14.6569, 9., 3.34315, 1.}
	, TestID -> "ProbeGetEnergies-DiracSq-3"
	, SameTest -> (Norm[#1-#2]<(10^-4)&)
];
VerificationTest[ProbeGetEnergies[{1, 1, 1}]
	, {13.2915, 7.4641, 2.7085, 0.535898}
	, TestID -> "ProbeGetEnergies-DiracSq-4"
	, SameTest -> (Norm[#1-#2]<(10^-4)&)
];