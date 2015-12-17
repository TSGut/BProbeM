(* ::Package:: *)

VerificationTest[MatrixRepSU2[1],
	{{{0}},{{0}},{{0}}},
	TestID->"MatrixRepSU2-1"
];
VerificationTest[MatrixRepSU2[2],
	{{{0,1/2},{1/2,0}},{{0,-(I/2)},{I/2,0}},{{1/2,0},{0,-(1/2)}}},
	TestID->"MatrixRepSU2-2"
];
VerificationTest[MatrixRepSU2[3],
	{{{0,1/Sqrt[2],0},{1/Sqrt[2],0,1/Sqrt[2]},{0,1/Sqrt[2],0}},{{0,-(I/Sqrt[2]),0},{I/Sqrt[2],0,-(I/Sqrt[2])},{0,I/Sqrt[2],0}},{{1,0,0},{0,0,0},{0,0,-1}}},
	TestID->"MatrixRepSU2-3"
];
VerificationTest[MatrixRepSU2[4],
	{{{0,Sqrt[3]/2,0,0},{Sqrt[3]/2,0,1,0},{0,1,0,Sqrt[3]/2},{0,0,Sqrt[3]/2,0}},{{0,-((I Sqrt[3])/2),0,0},{(I Sqrt[3])/2,0,-I,0},{0,I,0,-((I Sqrt[3])/2)},{0,0,(I Sqrt[3])/2,0}},{{3/2,0,0,0},{0,1/2,0,0},{0,0,-(1/2),0},{0,0,0,-(3/2)}}},
	TestID->"MatrixRepSU2-4"
];
VerificationTest[MatrixRepSU2[5],
	{{{0,1,0,0,0},{1,0,Sqrt[3/2],0,0},{0,Sqrt[3/2],0,Sqrt[3/2],0},{0,0,Sqrt[3/2],0,1},{0,0,0,1,0}},{{0,-I,0,0,0},{I,0,-I Sqrt[3/2],0,0},{0,I Sqrt[3/2],0,-I Sqrt[3/2],0},{0,0,I Sqrt[3/2],0,-I},{0,0,0,I,0}},{{2,0,0,0,0},{0,1,0,0,0},{0,0,0,0,0},{0,0,0,-1,0},{0,0,0,0,-2}}},
	TestID->"MatrixRepSU2-5"
];
VerificationTest[MatrixRepSU2[{1,1}],
	MatrixRepSU2[{1,1}],
	TestID->"MatrixRepSU2-invalidparameter"
];