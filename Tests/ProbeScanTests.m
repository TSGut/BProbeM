ProbeInit[];

VerificationTest[ProbeScan[0.8]
	, ProbeScan[0.8]
	, TestID -> "ProbeScan-inited"
];



ProbeInit[PauliMatrix[{1,2,3}]];

(* just test options *)
VerificationTest[ ProbeScan[0.8]
	, Null
	, TestID -> "ProbeScan-wo-opts"
];
VerificationTest[ ProbeScan[0.8, Dimension->2]
	, Null
	, TestID -> "ProbeScan-Dimension"
];
VerificationTest[ ProbeScan[0.8, ReplacePoints->False, MaxEnergy->2.1]
	, Null
	, TestID -> "ProbeScan-ReplacePoints"
];
VerificationTest[ ProbeScan[0.8, MinimalSurface->True]
	, Null
	, TestID -> "ProbeScan-MinimalSurface"
];
VerificationTest[ ProbeScan[0.8, MaxEnergy->2.1]
	, Null
	, TestID -> "ProbeScan-MaxEnergy"
];
VerificationTest[ ProbeScan[0.8, MaxEV->0.1]
	, Null
	, TestID -> "ProbeScan-MaxEV"
];
VerificationTest[ ProbeScan[0.8, MaxGradient->0.1]
	, Null
	, TestID -> "ProbeScan-MaxGradient"
];
VerificationTest[ ProbeScan[0.8, UpdateInterval->0.5]
	, Null
	, TestID -> "ProbeScan-UpdateInterval"
];
VerificationTest[ ProbeScan[0.8, GradientTracker->True]
	, Null
	, TestID -> "ProbeScan-GradienTracker"
];
VerificationTest[ ProbeScan[0.8, EnergyTracker->True]
	, Null
	, TestID -> "ProbeScan-EnergyTracker"
];
VerificationTest[ ProbeScan[0.8, EVTracker->True]
	, Null
	, TestID -> "ProbeScan-EVTracker"
];
VerificationTest[ ProbeScan[0.8, Parallelize->False]
	, Null
	, TestID -> "ProbeScan-Parallelize"
];

(* test fuzzy sphere *)
ProbeInit[PauliMatrix[{1,2,3}],StartingPoint->{1,0,0}];
ProbeScan[0.8];

VerificationTest[ProbeGetPoints[]
	, {{1,0,0},{0.780869,0.624695,-5.55112*10^-17},{0.780869,-0.624695,-5.55112*10^-17},{0.780869,0.,0.624695},{0.780869,0.,-0.624695},{0.609756,0.487805,0.624695},{0.609756,0.487805,-0.624695},{0.219367,0.975642,9.33698*10^-14},{0.609756,-0.487805,0.624695},{0.609756,-0.487805,-0.624695},{0.219367,-0.975642,9.33698*10^-14},{0.219367,0.,0.975642},{0.219367,0.,-0.975642},{0.244928,0.925804,0.287919},{0.0382992,0.358363,0.932796},{0.244928,0.925804,-0.287919},{0.0382992,0.358363,-0.932796},{0.171297,0.761849,0.624695},{0.171297,0.761849,-0.624695},{-0.437968,0.898991,-1.80966*10^-14},{0.244928,-0.925804,0.287919},{0.0382992,-0.358363,0.932796},{0.244928,-0.925804,-0.287919},{0.0382992,-0.358363,-0.932796},{0.171297,-0.761849,0.624695},{0.171297,-0.761849,-0.624695},{-0.437968,-0.898991,-1.80966*10^-14},{-0.437968,-3.74406*10^-15,0.898991},{-0.437968,3.74406*10^-15,-0.898991},{-0.315716,0.743765,0.589183},{-0.139886,0.958248,-0.249385},{-0.593727,0.313644,0.741023},{-0.315716,0.743765,-0.589183},{-0.139886,0.958248,0.249385},{-0.593727,0.313644,-0.741023},{0.74884,0.527205,0.401614},{0.74884,0.527205,-0.401614},{-0.903728,0.428108,4.13614*10^-13},{-0.315716,-0.743765,0.589183},{-0.139886,-0.958248,-0.249385},{-0.593727,-0.313644,0.741023},{-0.315716,-0.743765,-0.589183},{-0.139886,-0.958248,0.249385},{-0.593727,-0.313644,-0.741023},{0.74884,-0.527205,0.401614},{0.74884,-0.527205,-0.401614},{-0.903728,-0.428108,4.13614*10^-13},{-0.903728,-7.99947*10^-13,0.428108},{-0.903728,-1.24485*10^-12,-0.428108},{-0.835869,0.468959,0.285306},{0.478633,0.779771,-0.403567},{-0.697181,0.716756,0.0141222},{-0.301544,0.572199,-0.762666},{-0.835869,0.468959,-0.285306},{0.478633,0.779771,0.403567},{-0.301544,0.572199,0.762666},{0.492615,0.125302,0.86118},{0.676855,0.697988,-0.233838},{0.676855,0.697988,0.233838},{0.492615,0.125302,-0.86118},{-0.835869,-0.468959,0.285306},{0.478633,-0.779771,-0.403567},{-0.697181,-0.716756,0.0141222},{-0.301544,-0.572199,-0.762666},{-0.835869,-0.468959,-0.285306},{0.478633,-0.779771,0.403567},{-0.301544,-0.572199,0.762666},{0.492615,-0.125302,0.86118},{0.676855,-0.697988,-0.233838},{0.676855,-0.697988,0.233838},{0.492615,-0.125302,-0.86118},{-0.987547,-0.13924,0.073229},{-0.538075,0.553537,0.635667},{-0.550739,0.565848,-0.613598},{-0.992205,0.124102,0.0112742},{0.355434,0.621006,-0.698583},{-0.826548,0.272565,-0.492469},{-0.161613,-0.0348225,-0.98624},{0.355434,0.621006,0.698583},{-0.826548,0.272565,0.492469},{-0.161613,-0.0348225,0.98624},{-0.538075,-0.553537,0.635667},{-0.550739,-0.565848,-0.613598},{0.355434,-0.621006,-0.698583},{-0.826548,-0.272565,-0.492469},{0.355434,-0.621006,0.698583},{-0.826548,-0.272565,0.492469},{-0.771287,0.0681314,0.632831},{0.860154,0.305812,-0.408183},{-0.566679,0.801555,-0.190746},{-0.0564078,-0.647903,-0.759631},{-0.738842,-0.0943364,-0.667242},{0.860154,0.305812,0.408183},{-0.0564078,-0.647903,0.759631},{0.860154,-0.305812,-0.408183},{-0.566679,-0.801555,-0.190746},{0.860154,-0.305812,0.408183}}
	, TestID -> "ProbeScan-FuzzySphere"
	, SameTest -> Function[{list1,list2}, Block[{i, same},
		same = True;
		
		For[i=1,i<=Length[list1],i++,
			If[Norm[list1[[i]]-list2[[i]]] > 10^-6,
				same = False;
			];
		];
	
		Return[same];
	]]
];

ProbeInit[PauliMatrix[{1,2,3}],StartingPoint->{1,0,0},Parallelize->False];
ProbeScan[0.8];

VerificationTest[ProbeGetPoints[]
	, {{1,0,0},{0.780869,0.624695,-5.55112*10^-17},{0.780869,-0.624695,-5.55112*10^-17},{0.780869,0.,0.624695},{0.780869,0.,-0.624695},{0.609756,0.487805,0.624695},{0.609756,0.487805,-0.624695},{0.219367,0.975642,9.33698*10^-14},{0.609756,-0.487805,0.624695},{0.609756,-0.487805,-0.624695},{0.219367,-0.975642,9.33698*10^-14},{0.219367,0.,0.975642},{0.219367,0.,-0.975642},{0.244928,0.925804,0.287919},{0.0382992,0.358363,0.932796},{0.244928,0.925804,-0.287919},{0.0382992,0.358363,-0.932796},{0.171297,0.761849,0.624695},{0.171297,0.761849,-0.624695},{-0.437968,0.898991,-1.80966*10^-14},{0.244928,-0.925804,0.287919},{0.0382992,-0.358363,0.932796},{0.244928,-0.925804,-0.287919},{0.0382992,-0.358363,-0.932796},{0.171297,-0.761849,0.624695},{0.171297,-0.761849,-0.624695},{-0.437968,-0.898991,-1.80966*10^-14},{-0.437968,-3.74406*10^-15,0.898991},{-0.437968,3.74406*10^-15,-0.898991},{-0.315716,0.743765,0.589183},{-0.139886,0.958248,-0.249385},{-0.593727,0.313644,0.741023},{-0.315716,0.743765,-0.589183},{-0.139886,0.958248,0.249385},{-0.593727,0.313644,-0.741023},{0.74884,0.527205,0.401614},{0.74884,0.527205,-0.401614},{-0.903728,0.428108,4.13614*10^-13},{-0.315716,-0.743765,0.589183},{-0.139886,-0.958248,-0.249385},{-0.593727,-0.313644,0.741023},{-0.315716,-0.743765,-0.589183},{-0.139886,-0.958248,0.249385},{-0.593727,-0.313644,-0.741023},{0.74884,-0.527205,0.401614},{0.74884,-0.527205,-0.401614},{-0.903728,-0.428108,4.13614*10^-13},{-0.903728,-7.99947*10^-13,0.428108},{-0.903728,-1.24485*10^-12,-0.428108},{-0.835869,0.468959,0.285306},{0.478633,0.779771,-0.403567},{-0.697181,0.716756,0.0141222},{-0.301544,0.572199,-0.762666},{-0.835869,0.468959,-0.285306},{0.478633,0.779771,0.403567},{-0.301544,0.572199,0.762666},{0.492615,0.125302,0.86118},{0.676855,0.697988,-0.233838},{0.676855,0.697988,0.233838},{0.492615,0.125302,-0.86118},{-0.835869,-0.468959,0.285306},{0.478633,-0.779771,-0.403567},{-0.697181,-0.716756,0.0141222},{-0.301544,-0.572199,-0.762666},{-0.835869,-0.468959,-0.285306},{0.478633,-0.779771,0.403567},{-0.301544,-0.572199,0.762666},{0.492615,-0.125302,0.86118},{0.676855,-0.697988,-0.233838},{0.676855,-0.697988,0.233838},{0.492615,-0.125302,-0.86118},{-0.987547,-0.13924,0.073229},{-0.538075,0.553537,0.635667},{-0.550739,0.565848,-0.613598},{-0.992205,0.124102,0.0112742},{0.355434,0.621006,-0.698583},{-0.826548,0.272565,-0.492469},{-0.161613,-0.0348225,-0.98624},{0.355434,0.621006,0.698583},{-0.826548,0.272565,0.492469},{-0.161613,-0.0348225,0.98624},{-0.538075,-0.553537,0.635667},{-0.550739,-0.565848,-0.613598},{0.355434,-0.621006,-0.698583},{-0.826548,-0.272565,-0.492469},{0.355434,-0.621006,0.698583},{-0.826548,-0.272565,0.492469},{-0.771287,0.0681314,0.632831},{0.860154,0.305812,-0.408183},{-0.566679,0.801555,-0.190746},{-0.0564078,-0.647903,-0.759631},{-0.738842,-0.0943364,-0.667242},{0.860154,0.305812,0.408183},{-0.0564078,-0.647903,0.759631},{0.860154,-0.305812,-0.408183},{-0.566679,-0.801555,-0.190746},{0.860154,-0.305812,0.408183}}
	, TestID -> "ProbeScan-FuzzySphere-parallelize-off"
	, SameTest -> Function[{list1,list2}, Block[{i, same},
		same = True;
		
		For[i=1,i<=Length[list1],i++,
			If[Norm[list1[[i]]-list2[[i]]] > 10^-6,
				same = False;
			];
		];
	
		Return[same];
	]]
];

ProbeInit[PauliMatrix[{1,2,3}],StartingPoint->{1,0,0}];
ProbeScan[ 0.8, Dimension->1];
VerificationTest[ProbeGetPoints[]
	,{{1,0,0},{0.780869,0.,0.624695},{0.780869,0.,-0.624695},{0.219367,0.,0.975642},{0.219367,0.,-0.975642},{-0.437968,-3.74406*10^-15,0.898991},{-0.437968,3.74406*10^-15,-0.898991},{-0.903728,-7.99925*10^-13,0.428108},{-0.903728,-1.24486*10^-12,-0.428108}}
	, TestID -> "ProbeScan-FuzzySphere-dim-1"
	, SameTest -> Function[{list1,list2}, Block[{i, same},
		same = True;
		
		For[i=1,i<=Length[list1],i++,
			If[Norm[list1[[i]]-list2[[i]]] > 10^-6,
			same = False;
			];
		];
	
		Return[same];
	]]
];

ProbeInit[PauliMatrix[{1,2,3}],StartingPoint->{1,0,0}];
ProbeScan[0.8, ReplacePoints->False, MinimalSurface->True];
VerificationTest[ProbeGetPoints[]
	,{{1,0,0},{0.780869,0.624695,1.16359*10^-8},{0.780869,-0.624695,1.49627*10^-8},{0.780869,-3.49076*10^-8,0.624695},{0.780869,-2.41965*10^-8,-0.624695},{0.609756,0.487805,0.624695},{0.609756,0.487805,-0.624695},{0.219367,0.975642,-7.68139*10^-9},{0.609756,-0.487805,0.624695},{0.609756,-0.487805,-0.624695},{0.219367,-0.975642,1.44161*10^-8},{0.219367,-3.02597*10^-8,0.975642},{0.219367,-1.04208*10^-8,-0.975642},{0.244928,0.925804,0.287919},{0.0382992,0.358363,0.932796},{0.244928,0.925804,-0.287919},{0.0382992,0.358363,-0.932796},{0.171297,0.761849,0.624695},{0.171297,0.761849,-0.624695},{-0.437968,0.89899,-3.12243*10^-8},{0.244927,-0.925804,0.287919},{0.0382991,-0.358363,0.932796},{0.244928,-0.925804,-0.287919},{0.0382992,-0.358363,-0.932796},{0.171297,-0.761849,0.624695},{0.171297,-0.761849,-0.624695},{-0.437968,-0.898991,-1.01595*10^-8},{-0.437968,8.74425*10^-8,0.898991},{-0.437968,-2.24505*10^-8,-0.898991},{-0.315715,0.743765,0.589184},{-0.139886,0.958248,-0.249385},{-0.593727,0.313644,0.741023},{-0.315716,0.743765,-0.589183},{-0.139886,0.958248,0.249385},{-0.593727,0.313644,-0.741023},{0.74884,0.527205,0.401615},{0.74884,0.527205,-0.401614},{-0.903728,0.428108,-1.52304*10^-8},{-0.315717,-0.743765,0.589183},{-0.139885,-0.958248,-0.249386},{-0.593727,-0.313644,0.741023},{-0.315714,-0.743765,-0.589185},{-0.139887,-0.958248,0.249385},{-0.593727,-0.313644,-0.741023},{0.74884,-0.527205,0.401614},{0.74884,-0.527205,-0.401614},{-0.903728,-0.428108,-9.14289*10^-8},{-0.903728,2.56757*10^-7,0.428108},{-0.903728,-6.82516*10^-9,-0.428108},{-0.835869,0.468959,0.285306},{0.478633,0.779771,-0.403568},{-0.697181,0.716756,0.014123},{-0.301545,0.572199,-0.762666},{-0.835869,0.468959,-0.285306},{0.478633,0.779771,0.403567},{-0.301544,0.572199,0.762666},{0.492615,0.125302,0.861179},{0.676855,0.697988,-0.233837},{0.676855,0.697988,0.233838},{0.492615,0.125302,-0.86118},{-0.83587,-0.46896,0.285304},{0.478635,-0.779771,-0.403566},{-0.697181,-0.716756,0.0141202},{-0.301542,-0.572199,-0.762667},{-0.835869,-0.468959,-0.285308},{0.478632,-0.779771,0.403569},{-0.301548,-0.572199,0.762664},{0.492615,-0.125302,0.861179},{0.676855,-0.697988,-0.233838},{0.676855,-0.697988,0.233838},{0.492615,-0.125302,-0.861179},{-0.987547,-0.13924,0.0732289},{-0.538075,0.553536,0.635667},{-0.550739,0.565848,-0.613598},{-0.992205,0.124101,0.0112749},{0.355433,0.621007,-0.698583},{-0.826549,0.272564,-0.492469},{-0.161613,-0.0348224,-0.98624},{0.355433,0.621006,0.698583},{-0.826549,0.272565,0.492469},{-0.161613,-0.0348226,0.98624},{-0.538076,-0.553537,0.635665},{-0.550738,-0.565847,-0.6136},{0.355436,-0.621005,-0.698583},{-0.826547,-0.272566,-0.492471},{0.35543,-0.621009,0.698583},{-0.826551,-0.272563,0.492467},{-0.771286,0.0681299,0.632831},{0.860154,0.305814,-0.408183},{-0.56668,0.801555,-0.190745},{-0.0564086,-0.647903,-0.759631},{-0.738843,-0.0943359,-0.667242},{0.860154,0.305813,0.408183},{-0.0564077,-0.647903,0.759631},{0.860155,-0.30581,-0.408183},{-0.566678,-0.801556,-0.190746},{0.860152,-0.305817,0.408183}}
	,TestID->"ProbeScan-FuzzySphere-minimalsurface"
	, SameTest -> Function[{list1,list2}, Block[{i, same},
		same = True;
		
		For[i=1,i<=Length[list1],i++,
			If[Norm[list1[[i]]-list2[[i]]] > 10^-6,
			same = False;
			];
		];
	
		Return[same];
	]]
];

ProbeInit[PauliMatrix[{1,2,3}],StartingPoint->{1,0,0}];
ProbeScan[0.8, ReplacePoints->False, MaxEnergy->2.3];
VerificationTest[ProbeGetPoints[]
	,{{1,0,0},{1.,0.8,0.},{1.,-0.8,0.},{1.,0.,0.8},{1.,0.,-0.8},{1.,0.8,0.8},{1.,0.8,-0.8},{0.500105,1.42458,-4.26609*10^-12},{1.,-0.8,0.8},{1.,-0.8,-0.8},{0.500105,-1.42458,-4.26609*10^-12},{0.500105,0.,1.42458},{0.500105,0.,-1.42458}}
	,TestID->"ProbeScan-FuzzySphere-MaxEnergy"
	, SameTest -> Function[{list1,list2}, Block[{i, same},
		same = True;
		
		For[i=1,i<=Length[list1],i++,
			If[Norm[list1[[i]]-list2[[i]]] > 10^-4,
			same = False;
			];
		];
	
		Return[same];
	]]
];

ProbeInit[PauliMatrix[{1,2,3}],StartingPoint->{1,0,0}];
ProbeScan[0.8, ReplacePoints->False, MaxGradient->1.1];
VerificationTest[ProbeGetPoints[]
	,{{1,0,0},{1.,0.8,0.},{1.,-0.8,0.},{1.,0.,0.8},{1.,0.,-0.8},{1.,0.8,0.8},{1.,0.8,-0.8},{0.500105,1.42458,-4.26609*10^-12},{1.,-0.8,0.8},{1.,-0.8,-0.8},{0.500105,-1.42458,-4.26609*10^-12},{0.500105,0.,1.42458},{0.500105,0.,-1.42458}}
	,TestID->"ProbeScan-FuzzySphere-MaxGradient"
	, SameTest -> Function[{list1,list2}, Block[{i, same},
		same = True;
		
		For[i=1,i<=Length[list1],i++,
			If[Norm[list1[[i]]-list2[[i]]] > 10^-4,
			same = False;
			];
		];
	
		Return[same];
	]]
];

ProbeInit[PauliMatrix[{1,2,3}],StartingPoint->{1,0,0}];
ProbeScan[0.8, MaxEV -> 0.001];
VerificationTest[ProbeGetPoints[]
	,{{1, 0, 0}}
	,TestID->"ProbeScan-FuzzySphere-MaxEV"
	, SameTest -> Function[{list1,list2}, Block[{i, same},
		same = True;
		
		For[i=1,i<=Length[list1],i++,
			If[Norm[list1[[i]]-list2[[i]]] > 10^-4,
			same = False;
			];
		];
	
		Return[same];
	]]
];