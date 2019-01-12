structure tigercodegen :> tigercodegen =
struct

open tigerframe
open tigerassem
open tigertree
open tigertemp
(* codegen: Pagina 206 *)
fun codegen frame stm =
let val ilist = ref ([]:instr list)
		fun emit x = ilist := x::!ilist
		fun result gen = let val t = tigertemp.newtemp() in (gen t; t) end
		(* munchStm: Tree.stm -> unit
     * Emits assembly to execute the given statement. *)
    (* Pagina 204 *)
    fun munchStm (MOVE (TEMP t1, e)) = (*COMPLETAR*)
      | munchStm (MOVE (MEM e1, e2)) = (*COMPLETAR*)
      | munchStm (MOVE (e1, e2)) = (*COMPLETAR*)
      | munchStm (EXP (CALL (NAME n, args))) = (*COMPLETAR*)
      | munchStm (EXP (CALL _)) = (*COMPLETAR*)
      | munchStm (EXP e) = (*COMPLETAR*)
      | munchStm (JUMP (NAME n, lst)) = (*COMPLETAR*)
      | munchStm (JUMP _) = (*COMPLETAR*)
      | munchStm (CJUMP (oper, e1, e2, l1, l2)) = (*COMPLETAR*)
      | munchStm (SEQ (e1, e2)) = (*COMPLETAR*)
      | munchStm (LABEL l) = (*COMPLETAR*)
		(* munchExp: Tree.exp -> Temp.temp
     * Emits assembly to evaluate the expression and returns the register where
     * the result is saved. *)
    (* Pagina 205 *)
    fun munchExp (CONST n) = (*COMPLETAR*)
      | munchExp (TEMP t) = (*COMPLETAR*)
      | munchExp (BINOP (PLUS, e1, e2)) = (*COMPLETAR*)
      | munchExp (BINOP (MINUS, e1, e2)) = (*COMPLETAR*)
      | munchExp (BINOP (MUL, e1, e2)) = (*COMPLETAR*)
      | munchExp (BINOP (DIV, e1, e2)) = (*COMPLETAR*)
      | munchExp (BINOP _) = (*COMPLETAR*)
      | munchExp (MEM e) = (*COMPLETAR*)
      | munchExp (CALL _) = (*COMPLETAR*)
      | munchExp (NAME n) = (*COMPLETAR*)
      | munchExp (ESEQ _) = (*COMPLETAR*)
		(* munchArgs: Tree.exp list -> Temp.temp list
	   * Push all the argument to the registers and the stack according to the
	   * calling convention. *)
		fun muchArgs args = (*COMPLETAR*)
in munchStm stm; rev(!ilist) end
