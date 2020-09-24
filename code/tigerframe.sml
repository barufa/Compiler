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

val fpPrev = 0				 (* offset (bytes) *)
val fpPrevLev = 16	         (* offset (bytes) *)
val wSz = 8				     (* word size in bytes *)
val log2WSz = 3				 (* base two logarithm of word size in bytes *)
(*
val fpPrevLev = ~wSz	     (* offset (bytes) *)
val wSz = 8				     (* word size in bytes *)
val log2WSz = 3				 (* base two logarithm of word size in bytes *)
*)

val localsInicial = 0	 (* words *)
val localsGap = ~4 		 (* bytes *)

(* COMPLETAR registros *)
val rv = "rax"      (* return value  *)
val fp = "rbp"      (* frame pointer *)
val sp = "rsp"      (* stack pointer *)
(* val ov  = "rdx"  (* overflow value *) *)
(* Revisar callersaves*)
val specialregs = [fp, sp]                                                        (* special purpose registers *)
val argsregs = ["rdi", "rsi", "rcx", "rdx", "r8", "r9"]                           (* registers that hold arguments *)
val callersaves = ["rax", "rcx", "rdx", "rsi", "rdi", "r8", "r9"]                 (* registers that must be preserved by the caller *)
val calleesaves = ["rbx", "r10", "r11", "r12", "r13", "r14", "r15"]               (* registers that must be preserved by the callee *)
val machineregs = callersaves @ calleesaves                                       (* all registers available for coloring *)

fun allocMem(k) = InFrame k

fun allocLocal (f: frame) b =
	case b of
	  true =>
		let val _ = #actualLocal f:=(!(#actualLocal f)+1)
		in InFrame(!(#actualLocal f)*wSz+localsGap) end
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
  | exp(InReg l)   e = TEMP l
fun build_seq [] = EXP (CONST 0)
  | build_seq [s] = s
  | build_seq (x::xs) = SEQ (x, build_seq xs)
fun externalCall(s, l) = CALL(NAME s, l)

fun procEntryExit1 (f: frame,body) = (*body*)(*COMPLETADO*)
    let val frame_name = (name f)
        fun move_args [] _     = []
          | move_args (x::xs) n = let val argsregs_size = List.length argsregs
                                     val dst = if n < argsregs_size then TEMP (List.nth(argsregs,n))
                                               else MEM(BINOP(PLUS, CONST ((n-argsregs_size)*8+16), TEMP fp))
                                 in MOVE(exp x fp, dst) :: (move_args xs (n+1))  end
        val args_connect = [COMMENT("Cargando los argumentos")] @ (move_args (!(#accesslist f)) 0)
        val new_temps = List.tabulate(List.length calleesaves , fn _ => TEMP (tigertemp.newtemp()))
        val save_calleesaves = if frame_name = "_tigermain" then [] (*Es necesario guardar los callesaves? No se hace durante el coloreo?*)
                               else [COMMENT("Guardando registros calleesaves")] @ List.map MOVE(ListPair.zip(new_temps,List.map TEMP calleesaves))
        val restore_calleesaves = if frame_name = "_tigermain" then []
                                  else [COMMENT("Restaurando registros calleesaves")] @ List.map MOVE(ListPair.zip(List.map TEMP calleesaves,new_temps))
    in build_seq(save_calleesaves @ args_connect @ [body] @ restore_calleesaves) end

fun procEntryExit2 (frame,instr) = (*COMPLETADO*)
	instr @ [tigerassem.IOPER {assem = "",dst = callersaves,src = calleesaves,jump = NONE}]

fun procEntryExit3 (frame,instr) =(*COMPLETADO*)
  let
    val tam = (!(#actualLocal frame) * wSz)
    val size = ((tam + 15) div 16) * 16 (*Tamaño del frame*)
  in {prolog = ".global " ^ name(frame) ^ "\n" ^
                 "\t" ^ name(frame) ^ ":\n" ^
                 "#Prologo\n" ^
                 "\tpushq %rbp\n" ^
                 "\tmovq %rsp, %rbp\n" ^
                 "\tsubq $" ^ Int.toString(size) ^", %rsp\n#Cuerpo\n",
      body = instr,
      epilog = "#Epilogo\n" ^
               "\tmovq %rbp, %rsp\n" ^
               "\tpopq %rbp\n" ^
               "\tret\n\n"}
  end

end
