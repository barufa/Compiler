structure tigercodegen :> tigercodegen =
struct

open tigerframe
open tigerassem
open tigertree
open tigertemp
(* codegen: Pagina 206 *)
fun codegen frame stm =
let val ilist = ref ([]:instr list)
		fun result gen = let val t = tigertemp.newtemp() in (gen t; t) end
		fun emit x = ilist := let val _ = (formatCode x) in (x::(!ilist)) end
		fun emitM (ass,src,dst) = emit(IMOVE{assem = ass,src = src,dst = dst})
		fun emitO (ass,src,dst,jmp) = emit(IOPER{assem=ass,src=src,dst=dst,jump=jmp})
		fun emitL (lab) = emit(ILABEL {assem=lab^":", lab=lab})
		(* fun emit x = ilist := (x::(!ilist)) end *)

		(* munchStm: Tree.stm -> Unit
     * Emits assembly to execute the given statement. *)
    (* Pagina 204 *)
    fun munchStm (SEQ (e1, e2)) = (munchStm e1; munchStm e2)
			| munchStm (MOVE (TEMP t1, ex)) =
					emitM("movq %'s0, %'d0",munchExp ex,t1)
      | munchStm (MOVE (MEM e1, e2)) =
					emitO("movq %'s0, (%'s1)",[munchExp e2,munchExp e1],[],NONE)
			| munchStm (LABEL l) = emitL(l)
      | munchStm (JUMP (NAME n, ln)) =
					emitO ("jmp "^n,[],[],SOME ln)
      | munchStm (CJUMP (oper, e1, e2, l1, l2)) =
					let val _ = emitO("cmpq %'s0, %'s1",[munchExp e1, munchExp e2],[],NONE)
					in case oper of
						  EQ => emitO("je 'j0",[],[],SOME [l1,l2])
						| NE => emitO("jne 'j0",[],[],SOME [l1,l2])
						| LT => emitO("jl 'j0",[],[],SOME [l1,l2])
						| GT => emitO("jg 'j0",[],[],SOME [l1,l2])
						| LE => emitO("jle 'j0",[],[],SOME [l1,l2])
						| GE => emitO("jge 'j0",[],[],SOME [l1,l2])
						| _  => raise Fail "Operacion erronea en jump condicional"
					end
			| munchStm (EXP (CALL (NAME n, args))) = (*COMPLETAR*)
					emitO("call "^n,muchArgs args,calldefs,NONE)
			| munchStm (EXP e) = (munchExp e; ())
			| munchStm _ = raise Fail "Casos no cubiertos en tigercodegen.munchStm"


		(* munchExp: Tree.exp -> Temp.temp
     * Emits assembly to evaluate the expression and returns the register where
     * the result is saved. *)
    (* Pagina 205 *)
    and munchExp (CONST n) =
				result ( fn r => emitO("movq $"^(Int.toString n)^", %'d0",[],[r],NONE))
      | munchExp (TEMP t) = t
      | munchExp (BINOP (PLUS, e1, e2)) =
				result ( fn r => (emitM("movq %'s0, %'d0",munchExp e1,r);
												 emitO("addq %'s1, %'d0",[r, munchExp e2],[r],NONE)))
      | munchExp (BINOP (MINUS, e1, e2)) =
				result ( fn r => (emitM("movq %'s0, %'d0",munchExp e1,r);
												 emitO("subq %'s1, %'d0",[r, munchExp e2],[r],NONE)))
      | munchExp (BINOP (MUL, e1, e2)) = (*Revisar!!!!!!!!*)
				result ( fn r =>
					(emitM("movq %'s0, %'d0",munchExp e1,rv);
					emitO("imulq %'s1",[rv, munchExp e2],[rv,ov],NONE);
					emitM("movq %'s0, %'d0",rv,r)))
      | munchExp (BINOP (DIV, e1, e2)) =
				result ( fn r =>
					 		(emitM("movq %'s0, %'d0",munchExp e1,rv);
							emitO("cdqo",[rv],[ov],NONE);
							emitO("idivq %'s2",[rv,ov,munchExp e2],[rv,ov],NONE);
							emitM("movq %'s0, %'d0",rv,r)))
      | munchExp (MEM e) =
				result (fn r => emitO("movq (%'s0), %'d0",[munchExp e],[r],NONE))
      | munchExp (NAME n) =
				result (fn r => emitO("movq "^n^" %'d0",[],[r],NONE))
			| munchExp (CALL f) =
				result (fn r => (munchStm(EXP(CALL f));emitM("movq %'s0, %'d0",rv,r)))
			| munchExp exp = raise Fail "Casos no cubiertos en tigercodegen.munchExp"


		(* munchArgs: Tree.exp list -> Temp.temp list
	   * Push all the argument to the registers and the stack according to the
	   * calling convention. *)
		and muchArgs args = (*COMPLETAR*)
			let fun muchArgsaux (x::arg) (r::argregs) =(*Mover x a r*)
								let val _ = munchStm(MOVE (TEMP r, x))
								in r ::(muchArgsaux arg argregs) end
						| muchArgsaux (x::arg) [] =(*Mover x a la pila*)
								let val _ = emit(IOPER{assem = "pushq %'d0", src = [munchExp x], dst = [], jump = NONE})
								in  muchArgsaux arg [] end
						| muchArgsaux [] _ = [](*Finaliza la funcion*)
			in muchArgsaux args argsregs end
in munchStm stm; rev(!ilist) end

end
