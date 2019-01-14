struct
structure tigercodegen :> tigercodegen =

open tigerframe
open tigerassem
open tigertree
open tigertemp
(* codegen: Pagina 206 *)
fun codegen frame stm =
let val ilist = ref ([]:instr list)
		fun emit x = ilist := x::!ilist
		fun result gen = let val t = tigertemp.newtemp() in (gen t; t) end
		(* munchStm: Tree.stm -> Unit
     * Emits assembly to execute the given statement. *)
    (* Pagina 204 *)
    fun munchStm (MOVE (TEMP t1, e)) = (*COMPLETAR*)
				emit (MOV{assem = "movq %'s0, %'d0", src=munchExp e, dst=t1})
      | munchStm (MOVE (MEM e1, e2)) =
				emit (OPER{assem = "movq %'s0, (%'s1)", src=[munchExp e2,munchExp e1],dst=[],jump=NONE})
      | munchStm (EXP (CALL (NAME n, args))) = (*COMPLETAR*)
      | munchStm (EXP (CALL _)) = (*COMPLETAR*)
      | munchStm (EXP e) = (munchExp e; ())
      | munchStm (JUMP (NAME n, ln)) =
				emit(OPER{assem="jmp "^n, src=[], dst=[], jump=SOME ln})
      | munchStm (JUMP _) =
				raise Fail "Jump sin etiqueta"
      | munchStm (CJUMP (oper, e1, e2, l1, l2)) =
				let val _ = emit(OPER{assem = "cmpq 's0, 's1", src=[munchExp e1, munchExp e2], dst=[], jump=NONE})
				in case oper of
						EQ => emit(OPER{assem="je 'j0", src=[], dst=[], jump=SOME [l1,l2]})
					| NE => emit(OPER{assem="jne 'j0", src=[], dst=[], jump=SOME [l1,l2]})
					| LT => emit(OPER{assem="jl 'j0", src=[], dst=[], jump=SOME [l1,l2]})
					| GT => emit(OPER{assem="jg 'j0", src=[], dst=[], jump=SOME [l1,l2]})
					| LE => emit(OPER{assem="jle 'j0", src=[], dst=[], jump=SOME [l1,l2]})
					| GE => emit(OPER{assem="jge 'j0", src=[], dst=[], jump=SOME [l1,l2]})
					| _  => raise Fail "Operacion erronea en jump condicional"
      | munchStm (SEQ (e1, e2)) = (munchStm e1; munchStm e2)
      | munchStm (LABEL l) = emit(LABEL {assem=l^":", lab=l})
			| munchStm exp = raise Fail "Casos no cubiertos en tigercodegen.munchStm"
		(* munchExp: Tree.exp -> Temp.temp
     * Emits assembly to evaluate the expression and returns the register where
     * the result is saved. *)
    (* Pagina 205 *)
    fun munchExp (CONST n) =
				result (fn r => emit(OPER{assem = "movq $"^(toString i)^", %'d0", src = [], dst = [r], jump = NONE}))
      | munchExp (TEMP temp_reg) = temp_reg
      | munchExp (BINOP (PLUS, e1, e2)) =
				result ( fn r => (emit(MOV{assem = "movq %'s0, %'d0", src=munchExp e1, dst=r}); emit(OPER{assem = "addq %'s1, %'d0", src = [r, munchExp e2], dst = [r], jump = NONE})))
      | munchExp (BINOP (MINUS, e1, e2)) =
				result ( fn r => (emit(MOV{assem = "subq %'s0, %'d0", src=munchExp e1, dst=r}); emit(OPER{assem = "addq %'s1, %'d0", src = [r, munchExp e2], dst = [r], jump = NONE})))
      | munchExp (BINOP (MUL, e1, e2)) =
				result ( fn r => (emit(MOV{assem = "imulq %'s0, %'d0", src=munchExp e1, dst=r}); emit(OPER{assem = "addq %'s1, %'d0", src = [r, munchExp e2], dst = [r], jump = NONE})))
      | munchExp (BINOP (DIV, e1, e2)) =
				result ( fn r =>
					let val te2 = munchExp e2
					in 	munchStm (MOVE (TEMP RAX, e1));
							emit(OPER{assem = "cqto", src=[RAX], dst=[RDX], jump = NONE});
							emit(OPER{assem = "idivq %'s2", src = [RAX,RDX,m2], dst = [RAX,RDX], jump = NONE});
							munchStm (MOVE (TEMP r, TEMP RAX)))
      | munchExp (BINOP _) = raise Fail "Operacion binaria no soportada"
      | munchExp (MEM e) =
				result (fn r => emit(OPER{assem = "movq %'s0, %'d0", src=[munchExp e] , dst=[r], jump=NONE}))
      | munchExp (NAME n) =
				result (fn r => munchStm (MOVE (TEMP r, NAME n)))
			| munchExp exp = raise Fail "Casos no cubiertos en tigercodegen.munchExp"
		(* munchArgs: Tree.exp list -> Temp.temp list
	   * Push all the argument to the registers and the stack according to the
	   * calling convention. *)
		fun muchArgs args = (*COMPLETAR*)
			let fun muchArgsaux (x:arg) (r:argregs) =(*Mover x a r*)
								let val _ = munchStm(MOVE (TEMP r, x))
								in muchArgsaux arg argregs end
						| muchArgsaux (x:arg) [] =(*Mover x a la pila*)
								let val _ = emit(OPER{assem = "pushq 'd0", src = [munchExp x], dst = [], jump = NONE})
								in  muchArgsaux arg [] end
						| muchArgsaux [] _ = ()(*Finaliza la funcion*)
			in muchArgsaux args argsregs end
in munchStm stm; rev(!ilist) end
