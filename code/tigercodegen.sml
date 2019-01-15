structure tigercodegen :> tigercodegen =
struct

open tigerframe
open tigerassem
open tigertree
open tigertemp
(* codegen: Pagina 206 *)
fun codegen frame stm =
let val ilist = ref ([]:instr list)
		(* fun emit x = ilist := (x::(!ilist)) end *)
		fun emit x = ilist := let val _ = (formatCode x) in (x::(!ilist)) end
		fun result gen = let val t = tigertemp.newtemp() in (gen t; t) end
		(* munchStm: Tree.stm -> Unit
     * Emits assembly to execute the given statement. *)
    (* Pagina 204 *)
    fun munchStm (MOVE (TEMP t1, e)) = (*COMPLETAR*)
				emit (IMOVE{assem = "movq %'s0, %'d0", src=(munchExp e), dst=t1})
      | munchStm (MOVE (MEM e1, e2)) =
				emit (IOPER{assem = "movq %'s0, %'s1", src=[munchExp e2,munchExp e1],dst=[],jump=NONE})
      | munchStm (EXP (CALL (NAME n, args))) = () (*COMPLETAR*)
      | munchStm (EXP (CALL _)) = ()(*COMPLETAR*)
      | munchStm (EXP e) = (munchExp e; ())
      | munchStm (JUMP (NAME n, ln)) =
				emit(IOPER{assem="jmp "^n, src=[], dst=[], jump=SOME ln})
      | munchStm (JUMP _) =
				raise Fail "Jump sin etiqueta"
      | munchStm (CJUMP (oper, e1, e2, l1, l2)) =
				let val _ = emit(IOPER{assem = "cmpq 's0, 's1", src=[munchExp e1, munchExp e2], dst=[], jump=NONE})
				in case oper of
					  EQ => emit(IOPER{assem="je 'j0", src=[], dst=[], jump=SOME [l1,l2]})
					| NE => emit(IOPER{assem="jne 'j0", src=[], dst=[], jump=SOME [l1,l2]})
					| LT => emit(IOPER{assem="jl 'j0", src=[], dst=[], jump=SOME [l1,l2]})
					| GT => emit(IOPER{assem="jg 'j0", src=[], dst=[], jump=SOME [l1,l2]})
					| LE => emit(IOPER{assem="jle 'j0", src=[], dst=[], jump=SOME [l1,l2]})
					| GE => emit(IOPER{assem="jge 'j0", src=[], dst=[], jump=SOME [l1,l2]})
					| _  => raise Fail "Operacion erronea en jump condicional"
				end
      | munchStm (SEQ (e1, e2)) = (munchStm e1; munchStm e2)
      | munchStm (LABEL l) = emit(ILABEL {assem=l^":", lab=l})
			| munchStm exp = raise Fail "Casos no cubiertos en tigercodegen.munchStm"
		(* munchExp: Tree.exp -> Temp.temp
     * Emits assembly to evaluate the expression and returns the register where
     * the result is saved. *)
    (* Pagina 205 *)
    and munchExp (CONST n) =
				result (fn r => emit(IOPER{assem = "movq $"^(Int.toString n)^", %'d0", src = [], dst = [r], jump = NONE}))
      | munchExp (TEMP temp_reg) = temp_reg
      | munchExp (BINOP (PLUS, e1, e2)) =
				result ( fn r => (emit(IMOVE{assem = "movq %'s0, %'d0", src=munchExp e1, dst=r}); emit(IOPER{assem = "addq %'s1, %'d0", src = [r, munchExp e2], dst = [r], jump = NONE})))
      | munchExp (BINOP (MINUS, e1, e2)) =
				result ( fn r => (emit(IMOVE{assem = "subq %'s0, %'d0", src=munchExp e1, dst=r}); emit(IOPER{assem = "addq %'s1, %'d0", src = [r, munchExp e2], dst = [r], jump = NONE})))
      | munchExp (BINOP (MUL, e1, e2)) =
				result ( fn r => (emit(IMOVE{assem = "imulq %'s0, %'d0", src=munchExp e1, dst=r}); emit(IOPER{assem = "addq %'s1, %'d0", src = [r, munchExp e2], dst = [r], jump = NONE})))
      | munchExp (BINOP (DIV, e1, e2)) =
				result ( fn r =>
					let val te2 = munchExp e2
					in 	munchStm (MOVE (TEMP rax, e1));
							emit(IOPER{assem = "cqto", src=[rax], dst=[rdx], jump = NONE});
							emit(IOPER{assem = "idivq %'s2", src = [rax,rdx,te2], dst = [rax,rdx], jump = NONE});
							munchStm (MOVE (TEMP r, TEMP rax))
					end)
      | munchExp (BINOP _) = raise Fail "Operacion binaria no soportada"
      | munchExp (MEM e) =
				result (fn r => emit(IOPER{assem = "movq %'s0, %'d0", src=[munchExp e] , dst=[r], jump=NONE}))
      | munchExp (NAME n) =
				result (fn r => munchStm (MOVE (TEMP r, NAME n)))
			| munchExp exp = raise Fail "Casos no cubiertos en tigercodegen.munchExp"
		(* munchArgs: Tree.exp list -> Temp.temp list
	   * Push all the argument to the registers and the stack according to the
	   * calling convention. *)
		fun muchArgs args = (*COMPLETAR*)
			let fun muchArgsaux (x::arg) (r::argregs) =(*Mover x a r*)
								let val _ = munchStm(MOVE (TEMP r, x))
								in r ::(muchArgsaux arg argregs) end
						| muchArgsaux (x::arg) [] =(*Mover x a la pila*)
								let val _ = emit(IOPER{assem = "pushq 'd0", src = [munchExp x], dst = [], jump = NONE})
								in  muchArgsaux arg [] end
						| muchArgsaux [] _ = [](*Finaliza la funcion*)
			in muchArgsaux args argsregs end
in munchStm stm; rev(!ilist) end

end
