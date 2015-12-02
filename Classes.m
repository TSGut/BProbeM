(* ::Package:: *)

(* :Title: Object-oriented Programming *)

(* :Name: Classes.m *)

(* :Author: Roman E. Maeder, June 1993. *)

(* :Summary:
This packages provides the support for object-oriented programming in Mathematica.
*)

(* :Context: MathProg`Classes` *)

(* :Package Version: 2.3 *)

(* :Copyright: Copyright 1993, Roman E. Maeder.

   Permission is granted to distribute verbatim copies of this package
   together with any of your packages that use it, provided the following
   acknowledgement is printed in a standard place:
 
	"Classes.m is distributed with permission by Roman E. Maeder."
  
   The newest release of Classes.m is available through MathSource.
*)

(* :History:
   Version 2.3 A SuperClass has to be protected, September 2008 - Goeran Andersson, goeran@kth.se
   Version 2.2 Bug fix for Class redefinition and Methods changed to InstanceMethods, September 2008 - Goeran Andersson, goeran@kth.se
   Version 2.1 Bug fix for Dot notation, September 2006 - Goeran Andersson, goeran@kth.se
   Version 2.0 Dot notation, ProtectClass and self replaced by this, May 2005 - Goeran Andersson, goeran@kth.se
   Version 1.2 for The Mathematica Programmer CD-ROM, March 1996
   Version 1.1, Improved efficiency, June 1993..
   Version 1.0 for the Mathematica Journal, October 1992.
*)

(* :Keywords: object-oriented programming, classes, methods, objects *)

(* :Source: 
    Maeder, Roman E.. 1993. Object-oriented Programming.
        The Mathematica Journal, 3(1).
*)

(* :Mathematica Version: 6.0 *)

(* :Limitation:
   There are no checks for the correct number of arguments of methods.
*)

(* :Discussion: 
   This version of the package is more efficient than the original one
   from the Journal. Objects use less space and are freed
   automatically, as soon as they are no longer referenced.  Therefore,
   delete[obj] should normally not be used any more.  The external
   representation of objects has been changed.  The functionality is
   the same, however.
   
   2005 - Dot notation implemented e.g. obj.method[args] instead of method[obj,args]
          self replaced by this
 

*)

BeginPackage["MathProg`Classes`"]

Class::usage = "Class[class, superclass, variables, methods] defines
	a new class as a subclass of superclass.
	Class[object] gives the class of an object."
Class::wrsym = "Class `1` is protected. Restart kernel to edit code."
ClassQ::usage = "ClassQ[symbol] is True, if symbol is a class."
InstanceMethods::usage = "InstanceMethods[class] gives the list of methods of class."
InstanceVariables::usage = "InstanceVariables[class] gives the
	list of instance variables of class."
SuperClass::usage = "SuperClass[class] gives the superclass of class."
SuperClass::unprtct = "A SuperClass has to be protected. Use ProtectClass[`1`]."
new::usage = "new[class, args...] generates a new object of class. Any
	method with name 'new' is called to initialize the new object."
delete::usage = "delete[obj] deletes an object (for special occasions only)."
this::usage = "this denotes the object inside methods."
super::usage = "super denotes the object as a member of its superclass."
isa::usage = "isa[obj, class] is true if obj belongs to class
	or a subclass of it."
NIM::usage = "NIM[this, <method>]& can be used as body of a
	pure virtual method."
ProtectClass::usage = "ProtectClass[class] protects the class from changes. Allways protect a super class since it cannot be changed."
	

Object::usage = "Object is the root class."

Object::nim = "Method `1` not implemented for class `2`."

Classes::info = "Classes.m is distributed with permission by Roman E. Maeder (March 1996)\nModified by Goeran Andersson, \
KTH, goeran@kth.se (September 2008)\nWarning: Dot[] overloaded for objecs \[DoubleLongRightArrow] method[object,data] \[DoubleLongLeftRightArrow] object.method[data]"


Begin["`Private`"]

(* :Overload Dot[] for objects - Goeran Andersson *)
Unprotect[Dot]; 
(object_)?(ClassQ[Class[#1]] & ) . (methods__) := Head[First[{methods}]][object, Sequence @@ First[{methods}]] . Sequence @@ Rest[{methods}];
Protect[Dot];  

(* :ProtectClass - Goeran Andersson *)
Unprotect[ProtectClass];
ProtectClass[(class_)?ClassQ] := (SetAttributes[class,{Protected,ReadProtected,Locked}]; class); 
Protect[ProtectClass];

Unprotect[ Class, InstanceMethods, InstanceVariables, SuperClass,
         new, delete, isa, this, NIM, ProtectClass ];

context = $Context;

(* private rules for instances *) {variables};
(* private class methods *)       {methodHandler};
(* other private symbols *)       {raise};

ClassQ[_] := False (* default *)

Class[ class_Symbol?ClassQ, args__]:=
	With[{context = Context[class],classString = ToString[class],classReferences = "*`" <> ToString[class]},
		If[MemberQ[ Attributes[class], Protected], Message[ClearAll::wrsym, class];Return[class], ClearAll[classReferences]];
		Class[ToExpression[context <> classString], args]
];

Class[ class_Symbol,
       superclass_?ClassQ,
       variables:{_Symbol...},
       methods:{{_Symbol, _Function}...}|{}
     ] :=
    Module[{apply, standard,
            localmethods, allvariables, messages,
            methodQ},

		(* goeran *)
		If[!MemberQ[Attributes[superclass], Protected], Return[ Message[SuperClass::unprtct, superclass] ] ];

        standard = { (* standard methods *)
            {Class,    class&},
            {isa, (class===#1 || isa[super, #1])&} };
        localmethods = Join[standard, methods];
        messages = Union[ Join[First /@ localmethods,
                               InstanceMethods[superclass]] ];
        allvariables = Join[variables, InstanceVariables[superclass]];
        (* class methods *)
        class/: InstanceMethods[class] = messages;
        class/: InstanceVariables[class] = allvariables;
        class/: SuperClass[class] = superclass;

        With[{ivars = Hold @@ allvariables,
              nvars = -Length[allvariables],
              localnames = First /@ localmethods,
              cookie = ToExpression[ context <> ToString[class] ],
              avars = allvariables},

          (* the head used for objects of this class *)
          SetAttributes[cookie, HoldAll];

          (* aux predicate for apply and message passing *)
          (methodQ[#] = True)& /@ messages;
          methodQ[_] = False;

          (* definitions of aux method applicator ``apply'' *)
          Apply[
            (apply[#1, obj_] :=
              With[{lvars = Take[Hold @@ obj, nvars]},
                #2 /.
                 List @@ Thread[ ivars :> lvars, Hold] /.
                 { this -> obj, super -> raise[obj, superclass] }
              ])&,
            localmethods, {1} ];
          (* inheritance, if not local method *)
          apply[f_, obj_] := methodHandler[superclass][f, obj];
          class/: methodHandler[class] = apply;

          (* message passing *)
          cookie/: (f_Symbol?methodQ)[obj_cookie, args___] :=
                   apply[f, obj][args];
          (* super *)
          raise/: (f_Symbol?methodQ)[raise[obj_, class], args___] :=
                   apply[f, obj][args];

          (* create instances of this class *)
          class/: new[class, init___] :=
            Module[{obj, syms = Unique[avars]},
	      SetAttributes[Evaluate[syms], Temporary];
              obj = cookie @@ syms;
              new[obj, init]; (* call any constructor defined *)
              obj
            ];
          (* formatting *)
          Format[obj_cookie] := StringForm["-`1`-", class]
        ];
        class/: ClassQ[class] = True; (* seal of approval *)
        class
    ]

(* the class Object: we need a dummy superclass *)

If[ !ClassQ[Object], (* goeran *)
Block[{noClass},
  noClass/: InstanceMethods[noClass] = {};
  noClass/: InstanceVariables[noClass] = {};
  noClass/: methodHandler[noClass] = badmessage;
  noClass/: SuperClass[noClass] = noClass;
  noClass/: ClassQ[noClass] = True;

  Protect[noClass]; (* goeran *)
  Class[ Object, noClass, {},
    {{new,               this&},
     {delete,            (Remove @@ this; Null)&},
     {isa,               #1===Object&},
     {InstanceMethods,           InstanceMethods[Class[this]]&},
     {InstanceVariables, InstanceVariables[Class[this]]&},
     {SuperClass,        SuperClass[Class[this]]&},
     {NIM,		 Message[Object::nim, #1, Class[this]]&}
    }
  ]
];
ProtectClass[ Object ] (* goeran *)
];

End[];

Protect[ Class, InstanceMethods, InstanceVariables, SuperClass,
         new, delete, isa, this, NIM ];

EndPackage[];
