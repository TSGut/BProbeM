ProbeInit[];

VerificationTest[ProbeGetEigenvalues[{1, 0, 0}]
	, ProbeGetEigenvalues[{1, 0, 0}]
	, TestID -> "ProbeGetEigenvalues-inited"
];


ProbeInit[PauliMatrix[{1,2,3}], Probe->"Laplace"];

VerificationTest[ProbeGetEigenvalues[{1, 0, 0}]
	, {6., 2.}
	, TestID -> "ProbeGetEigenvalues-Laplace-1"
];
VerificationTest[ProbeGetEigenvalues[{0, 0, 1}]
	, {6., 2.}
	, TestID -> "ProbeGetEigenvalues-Laplace-2"
];
VerificationTest[ProbeGetEigenvalues[{2, 0, 0}]
	, {11., 3.}
	, TestID -> "ProbeGetEigenvalues-Laplace-3"
];
VerificationTest[ProbeGetEigenvalues[{1, 1, 1}]
	, {9.4641, 2.5359}
	, TestID -> "ProbeGetEigenvalues-Laplace-4"
	, SameTest -> (Norm[#1-#2]<(10^-4)&)
];

ProbeInit[PauliMatrix[{1,2,3}], Probe->"DiracSq"];

VerificationTest[ProbeGetEigenvalues[{1, 0, 0}]
	, {10.4721, 4., 1.52786, -1.51909*10^-16}
	, TestID -> "ProbeGetEigenvalues-DiracSq-1"
	, SameTest -> (Norm[#1-#2]<(10^-4)&)
];
VerificationTest[ProbeGetEigenvalues[{0, 0, 1}]
	, {10.4721, 4., 1.52786, 0.}
	, TestID -> "ProbeGetEigenvalues-DiracSq-2"
	, SameTest -> (Norm[#1-#2]<(10^-4)&)
];
VerificationTest[ProbeGetEigenvalues[{2, 0, 0}]
	, {14.6569, 9., 3.34315, 1.}
	, TestID -> "ProbeGetEigenvalues-DiracSq-3"
	, SameTest -> (Norm[#1-#2]<(10^-4)&)
];
VerificationTest[ProbeGetEigenvalues[{1, 1, 1}]
	, {13.2915, 7.4641, 2.7085, 0.535898}
	, TestID -> "ProbeGetEigenvalues-DiracSq-4"
	, SameTest -> (Norm[#1-#2]<(10^-4)&)
];