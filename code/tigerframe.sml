(*
	Frames para el 80386 (sin displays ni registers).

		|    argn    |	fp+4*(n+1)
		|    ...     |
		|    arg2    |	fp+16
		|    arg1    |	fp+12
		|  fp level  |  fp+8
		|  retorno   |	fp+4
		|   fp ant   |	fp
		--------------	fp
		|   local1   |	fp-4
		|   local2   |	fp-8
		|    ...     |
		|   localn   |	fp-4*n
*)

structure tigerframe :> tigerframe = struct

open tigertree
datatype access = InFrame of int | InReg of tigertemp.label

type frame = {
	name: string,
	formals: bool list,
	locals: bool list,
	actualLocal: int ref,
	accesslist: access list ref
}
type register = string

datatype frag = PROC of {body: tigertree.stm, frame: frame}
	| STRING of tigertemp.label * string

(* COMPLETAR registros *)
val rv = "rax"      (* return value  *)
val fp = "rbp"      (* frame pointer *)
val sp = "rsp"      (* stack pointer *)
val ov  = "rdx"     (* overflow value *)
val rax = "rax"
val rdx = "rdx"

val fpPrev = 0				(* offset (bytes) *)
val fpPrevLev = ~WSz			(* offset (bytes) *)
val wSz = 8				(* word size in bytes *)
val log2WSz = 3				(* base two logarithm of word size in bytes *)
val calldefs = [rv]
val callersaves = []
val calleesaves = []


val argsregs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]

val callersaves = ["r10", "r11"] (* registers that must be preserved by the caller *)
val calleesaves = ["rbx", "r12", "r13", "r14", "r15"] (* registers that must be preserved by the callee *)
val calldefs = [RV] @ argregs @ callersaves
               (* registers possibly written by the callee *)
val machineRegs = specialregs @ argregs @ callersaves @ calleesaves
                  (* all registers available for coloring *)



val localsInicial = 0			(* words *)
val localsGap = ~4 			(* bytes *)
val specialregs = [rv, fp, sp] (* special purpose registers *)
val argregs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"] (* registers for passing first args *)
val callersaves = ["r10", "r11"] (* registers that must be preserved by the caller *)
val calleesaves = ["rbx", "r12", "r13", "r14", "r15"] (* registers that must be preserved by the callee *)
val calldefs = [rv] @ argregs @ callersaves (* registers possibly written by the callee *)
val machineRegs = specialregs @ argregs @ callersaves @ calleesaves

fun allocMem(k) = InFrame k

fun allocLocal (f: frame) b =
	case b of
	  true =>
		let
			val ret = InFrame(!(#actualLocal f)*wSz+localsGap)
			val _ = #actualLocal f:=(!(#actualLocal f)-1)
		in ret end
	| false => InReg(tigertemp.newtemp())
val allocArg = allocLocal

fun newFrame{name, formals} =
	{
		name=name,
		formals=formals,
		locals=[],
		actualLocal=ref localsInicial,
		accesslist = ref ([]:access list)
	}

fun name(f: frame) = #name f
fun formals({accesslist=l, ...}: frame) = !l (* COMPLETADO *)

fun acclist({accesslist=l, ...}:frame,acc: access) = let val _ = l:=(!l@[acc]) in acc end

fun string(l, s) = l^tigertemp.makeString(s)^"\n"
fun exp(InFrame k) e = MEM(BINOP(PLUS, TEMP(fp), CONST k))
	| exp(InReg l) e = TEMP l
fun externalCall(s, l) = CALL(NAME s, l)

fun procEntryExit1 (frame,body) = body (*COMPLETAR*)

fun procEntryExit2 (frame,instr) = (*COMPLETADO*)
	instr @ [tigerassem.OPER {assem = "",dst = [rv,sp,fp] @ caleesaves,src = [],jump = NONE}]

(*fun procEntryExit3 (frame,body) = body (*COMPLETAR*)
end*)
