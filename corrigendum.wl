(* ::Package:: *)

(* ::Title:: *)
(*Corrigendum*)


(* ::Author:: *)
(*Pedro Gomes Cabral \[LongDash] 2021, MIT Licensed.*)


(* ::Text:: *)
(*A package for robust error handling on the Wolfram Language.*)


BeginPackage["Corrigendum`"];


(* ::Subtitle:: *)
(**)


(* Unprotect older declared variables, clear all of them. *)
Unprotect[Evaluate[Context[] <> "*"]];
ClearAll[Evaluate[Context[] <> "*"]];


(* ::Subtitle:: *)
(**)


(* ::Section:: *)
(*Usage and Messages*)


Maybe::usage = "Maybe<T> is a metalinguistic function for a type that can hold a MaybeSome[T] or MaybeNone, use it as an argument for defining functions: Fn[Maybe[T_]]..";
MaybeSome::usage = "MaybeSome<T> is an enumerated type for Maybe that holds T.";
MaybeNone::usage = "MaybeNone is an enumerated type for Maybe that holds no value (theoretically, Nothing).";

Maybe::notpattern = "The argument of Maybe<T> must be a valid Pattern, such as \"Maybe[T_Integer]\".";

SomeQ::usage = "SomeQ[Enum_] returns True if Enum_ is a MaybeSome<T>, false otherwise.";
NoneQ::usage = "NoneQ[Enum_] returns True if Enum_ is a MaybeNone, false otherwise.";


Result::usage = "Result<T, E> is a metalinguistic function for a type that can hold ResultOk[T] or an ResultError[E], use it as an argument for defining functions: Fn[Result[T_, Err_]].";
ResultOk::usage = "ResultOk[T] is an enumerated type for Result<T, E> that holds a successful type T.";
ResultError::usage = "ResultError[T] is an enumerated type for Result<T, E> that holds an error E.";

Result::notpattern = "The arguments of Result<T, E> must be a valid Pattern, such as \"Result[T_Integer, Err_String]\".";

OkQ::usage = "OkQ[Enum_] returns True if Enum_ is a ResultOk<T>, false otherwise.";
ErrorQ::usage = "ErrorQ[Enum_] returns True if Enum_ is a ResultError<E>, false otherwise.";


MatchMaybe::usage = "MatchMaybe[Enum_, SomeExpr_, NoneExpr_] executes SomeExpr if Enum_ is MaybeSome<T>, otherwise, executes NoneExpr.";
MatchMaybe::invenum = "The first argument \"``\" is not a valid \"MaybeSome[T]\" or \"MaybeNone\".";


MatchResult::usage = "MatchResult[Enum_, OkExpr_, ErrorExpr_] executes OkayExpr if Enum_ is ResultOk<T>, otherwise, executes ErrorExpr.";
MatchResult::invenum = "The first argument \"``\" is not a valid \"ResultOk[T]\" or \"ResultError[E]\".";


Unwrap::usage = "Unwrap[T] returns the unwrapped version of T, such as MaybeSome[T] \[Rule] T, or ResultOk[T] \[Rule] T.";
UnwrapFlatten::usage = "UnwrapFlatten[Maybe<Maybe<T>>] returns the flattened and unwrapped version of T, such as MaybeSome[MaybeSome[T]] \[Rule] T, or ResultOk[ResultOk[T]] \[Rule] T.";

Unwrap::failed = "The expression \"``\" could not be Unwrapped. Perhaps \"``\" is not a Maybe or Result enumerated type.";
Unwrap::multiple = "Unwrap was called with multiple arguments, only one value may be unwrapped at the time.";
Unwrap::nothing = "Found literally nothing to Unwrap.";


AndThen::usage = "AndThen[Enum_, Function_] propagates Enum_ to the Function_ if Enum_ is a successful enumerated type. Best when used with Infix, such as: \"MaybeSome[5.0] ~ AndThen ~ (# ^ 2 &)\".";

AndThen::invenum = "The first argument not a Maybe or Result enumerated type.";
AndThen::checkusage = "Incorrect usage of piping function AndThen[Enum_, Function_], please check \"? AndThen\".";


IntoMaybe::usage = "IntoMaybe[Fn_, Args___] wraps the result of a given function into a \!\(\*
StyleBox[\"MaybeSome\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"<\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"T\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\">\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"or\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"MaybeNone\",\nFontWeight->\"Plain\"]\).";
IntoResult::usage = "IntoResult[Fn_, Args___] wraps the result of a given function into a \!\(\*
StyleBox[\"ResultOk\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"<\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"T\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\">\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"or\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"ResultError\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"<\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"E\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\">\",\nFontWeight->\"Plain\"]\).";


(* ::Subtitle:: *)
(**)


Begin["`Private`"];


(* ::Subtitle:: *)
(**)


(* ::Section:: *)
(*Maybe (with MaybeSome, MaybeNone)*)


MaybeSome = MaybeSome;


(* There's no such thing as None[T], since None holds no data at all. *)
MaybeNone[___] := MaybeNone;


MaybeNone = MaybeNone;


(* There's no such thing as None[T], since None holds no data at all. *)
MaybeNone[___] := MaybeNone;


(* Maybe[T] is a metaprogramming function. *)
Maybe[T_] := Module[{FP, SP},
	If[!StringMatchQ[ToString[T], WordCharacter.. ~~ "_" ~~ (WordCharacter.. | "")],
		Return[Message[Maybe::notpattern, ToString[T]]]
	];

	FP = StringSplit[ToString[T], "_"][[1]];
	SP = Quiet@Check["_" <> StringSplit[ToString[T], "_"][[-1]], "_"];

	Alternatives[
		ToExpression[FP <> "_MaybeSome /; MatchQ[" <> FP <> ", MaybeSome[" <> SP <> "]]"],
		ToExpression[FP <> "_ /; " <> FP <> " === MaybeNone"]
	]
];


Maybe[args___] := CompoundExpression[Message[Maybe::expr, ToString[args]], $Failed];


(* ::Subsection:: *)
(*Functions*)


SomeQ[MaybeSome[_]] := True;
SomeQ[_] := False;


NoneQ[MaybeNone] := True;
NoneQ[_] := False;


(* ::Subtitle:: *)
(**)


(* ::Section:: *)
(*Result (with ResultOk, ResultError)*)


ResultOk = ResultOk;


ResultError = ResultError;


(* Result[T, Err] is a metaprogramming function. *)
Result[T_, Err_] := Module[{FP, SP, EFP, ESP},
	If[!StringMatchQ[ToString[T], WordCharacter.. ~~ "_" ~~ (WordCharacter.. | "")],
		Return[Message[Result::notpattern, ToString[T]]]
	];
	
	If[!StringMatchQ[ToString[Err], WordCharacter.. ~~ "_" ~~ (WordCharacter.. | "")],
		Return[Message[Result::notpattern, ToString[Err]]]
	];

	FP = StringSplit[ToString[T], "_"][[1]];
	SP = Quiet@Check["_" <> StringSplit[ToString[T], "_"][[-1]], "_"];
	
	EFP = StringSplit[ToString[Err], "_"][[1]];
	ESP = Quiet@Check["_" <> StringSplit[ToString[Err], "_"][[-1]], "_"];

	Alternatives[
		ToExpression[FP <> "_ResultOk /; MatchQ[" <> FP <> ", ResultOk[" <> SP <> "]]"],
		ToExpression[EFP <> "_ResultError /; MatchQ[" <> EFP <> ", ResultError[" <> ESP <> "]]"]
	]
]


(* ::Subsection:: *)
(*Functions*)


OkQ[ResultOk[_]] := True;
OkQ[_] := False;


ErrorQ[ResultError[_]] := True;
ErrorQ[_] := False;


(* ::Subtitle:: *)
(**)


(* ::Section:: *)
(*MatchMaybe*)


SetAttributes[MatchMaybe, {HoldRest}];


MatchMaybe[MaybeSome[T_], SomeExpr_, NoneExpr_] := SomeExpr[T];


MatchMaybe[MaybeNone | MaybeNone[___], SomeExpr_, NoneExpr_] := NoneExpr[MaybeNone];


MatchMaybe[Any_, SomeExpr_, NoneExpr_] := CompoundExpression[Message[MatchMaybe::invenum, ToString[Any]], $Failed];


(* ::Subtitle:: *)
(**)


(* ::Section:: *)
(*MatchResult*)


SetAttributes[MatchResult, {HoldAll}];


MatchResult[T_ResultOk, OkExpr_, ErrorExpr_] := OkExpr[Unwrap[T]];


MatchResult[T_ResultError, OkExpr_, ErrorExpr_] := ErrorExpr[Unwrap[T]];


MatchResult[Any_, OkExpr_, ErrorExpr_] := CompoundExpression[Message[MatchResult::invenum, ToString[Any]], $Failed];


(* ::Subtitle:: *)
(**)


(* ::Section:: *)
(*Unwrap (UnwrapFlatten)*)


SetAttributes[Unwrap, {Listable}];


(* Unwrap[] := CompoundExpression[Message[Unwrap::nothing], $Failed]; *)
Unwrap[] := Nothing;


Unwrap[T_] := T;


Unwrap[MaybeSome[T_]] := T;
Unwrap[MaybeNone] := MaybeNone;


Unwrap[ResultOk[T_]] := T;
Unwrap[ResultError[E_]] := E;


Unwrap[args__] := CompoundExpression[Message[Unwrap::multiple], $Failed];


UnwrapFlatten = Unwrap@Flatten;


(* ::Subtitle:: *)
(**)


(* ::Section:: *)
(*AndThen (Chaining)*)


AndThen[T_MaybeSome | T_ResultOk, SomeExpr_Function] := SomeExpr[Unwrap[T]];


AndThen[(T_ /; T === MaybeNone) | T_ResultError, SomeExpr_Function] := Unwrap[T];


AndThen[T_, SomeExpr_Function] := T;


AndThen[___] := CompoundExpression[Message[AndThen::checkusage], $Failed];


(* ::Subtitle:: *)
(**)


(* ::Section:: *)
(*Flatten (UpSetDelayed)*)


Flatten[MaybeSome[T_MaybeSome]] ^:= Flatten[T];


Flatten[MaybeSome[T_]] ^:= MaybeSome[T];


(* ::Subtitle:: *)
(**)


(* ::Section:: *)
(*Safe Functions (IntoMaybe, IntoResult)*)


SetAttributes[IntoMaybe, HoldAll];
IntoMaybe[fn_, args__] := Quiet@Check[fn[Sequence[args]] /. {(Missing[___] | $Aborted | $Failed | Null) :> MaybeNone, any__ :> MaybeSome[Sequence[any]]}, MaybeNone];


SetAttributes[IntoResult, HoldAll];
IntoResult[fn_, args__] := Quiet@Check[ResultOk[fn[Sequence[args]]], ResultError[$MessageList]];


(* ::Subtitle:: *)
(**)


End[];


(* The great seal of protection! *)
Protect[Evaluate[Context[] <> "*"]];


EndPackage[];
