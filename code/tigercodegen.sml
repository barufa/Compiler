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
	fun emit x = ilist := (x::(!ilist))
	fun emitM (ass,src,dst) = emit(IMOVE{assem = ass,src = src,dst = dst})
	fun emitO (ass,src,dst,jmp) = emit(IOPER{assem=ass,src=src,dst=dst,jump=jmp})
	fun emitL (lab) = emit(ILABEL {assem=lab^":", lab=lab})
    (* munchStm: Tree.stm -> Unit
     * Emits assembly to execute the given statement. *)
    (* Pagina 204 *)
    fun munchStm (MOVE (TEMP t, e1)) = emitM("movq %'s0, %'d0",munchExp e1,t)
      | munchStm (MOVE (MEM e1, e2)) = emitO("movq %'s1, (%'s0)",[munchExp e1,munchExp e2],[],NONE)
      | munchStm (EXP (CALL (NAME n, args))) = munchCall n args
      | munchStm (EXP e) = (munchExp e; ())
      | munchStm (JUMP (NAME n, ln)) = emitO("jmp "^n,[],[],SOME ln)
      | munchStm (CJUMP (oper, e1, e2, l1, l2)) =
					let val _ = emitO("cmpq %'s1, %'s0",[munchExp e1, munchExp e2],[],NONE)
					in case oper of
						  EQ => emitO("je 'j0",[],[],SOME [l1,l2])
						| NE => emitO("jne 'j0",[],[],SOME [l1,l2])
						| LT => emitO("jl 'j0",[],[],SOME [l1,l2])
						| GT => emitO("jg 'j0",[],[],SOME [l1,l2])
						| LE => emitO("jle 'j0",[],[],SOME [l1,l2])
						| GE => emitO("jge 'j0",[],[],SOME [l1,l2])
						| _  => raise Fail "Operacion erronea en jump condicional"
					end
      | munchStm (SEQ (e1, e2)) = (munchStm e1; munchStm e2)
      | munchStm (LABEL l) = emitL(l)
      | munchStm (COMMENT c) = emit(tigerassem.IOPER {assem = "#"^c,dst = [],src = [],jump = NONE})
      | munchStm (PUSH r) = emit(tigerassem.IOPER {assem = "pushq %"^r,dst = [],src = [],jump = NONE})
      | munchStm (POP r) = emit(tigerassem.IOPER {assem = "popq %"^r,dst = [],src = [],jump = NONE})
      | munchStm _ = raise Fail "Casos no cubiertos en tigercodegen.munchStm"

	(* munchExp: Tree.exp -> Temp.temp
     * Emits assembly to evaluate the expression and returns the register where
     * the result is saved. *)
    (* Pagina 205 *)
    and munchExp (CONST n) = result ( fn r => emitO("movq $"^(Int.toString n)^", %'d0",[],[r],NONE))
      | munchExp (NAME n) = result (fn r => emitO("leaq "^n^"(%rip), %'d0",[],[r],NONE))
      | munchExp (TEMP t) = t
      | munchExp (BINOP (PLUS, e1, CONST n)) = result ( fn r => (emitM("movq %'s0, %'d0",munchExp e1,r);
                                                                 emitO("addq $"^Int.toString n^", %'d0",[r],[r],NONE)))
      | munchExp (BINOP (PLUS, CONST n, e2)) = munchExp (BINOP (PLUS, e2, CONST n))
      | munchExp (BINOP (PLUS, e1, e2)) = result ( fn r => (emitM("movq %'s0, %'d0",munchExp e1,r);
                                                            emitO("addq %'s1, %'d0",[r, munchExp e2],[r],NONE)))
      | munchExp (BINOP (MINUS, e1, CONST n)) = result ( fn r => (emitM("movq %'s0, %'d0",munchExp e1,r);
                                                                  emitO("subq $"^Int.toString n^", %'d0",[r],[r],NONE)))
      | munchExp (BINOP (MINUS, CONST n, e2)) = result ( fn r => (emitO("movq $"^Int.toString n^", %'d0",[],[r],NONE);
                             												              emitO("subq %'s1, %'d0",[r, munchExp e2],[r],NONE)))
      | munchExp (BINOP (MINUS, e1, e2)) = result ( fn r => (emitM("movq %'s0, %'d0",munchExp e1,r);
												             emitO("subq %'s1, %'d0",[r, munchExp e2],[r],NONE)))
      | munchExp (BINOP (MUL, e1, CONST n)) = result ( fn r => (emitM("movq %'s0, %'d0",munchExp e1,r);
                                                                emitO("imulq $"^Int.toString n^", %'d0",[r],[r],NONE)))
      | munchExp (BINOP (MUL, CONST n, e2)) = munchExp (BINOP (MUL, e2, CONST n))
      | munchExp (BINOP (MUL, e1, e2)) =
                result ( fn r =>
                    (emitM("movq %'s0, %'d0",munchExp e1,r);
                    emitO("imulq %'s1, %'d0",[r, munchExp e2],[r],NONE)))
      | munchExp (BINOP (DIV, e1, CONST n)) = result ( fn r => (emitM("movq %'s0, %'d0",munchExp e1,"rax");
                                                                emitO("movq $0, %'d0",[],["rdx"],NONE);
                                                                emitO("movq $"^Int.toString n^", %'d0",[],[r],NONE);
                                                                emitO("idivq %'s2, %rax",["rax","rdx",r],["rax","rdx"],NONE);
                                                                emitM("movq %'s0, %'d0","rax",r)))
      | munchExp (BINOP (DIV, CONST n, e2)) = result ( fn r => (emitO("movq $"^Int.toString n^", %'d0",[],["rax"],NONE);
                                                                emitO("movq $0, %'d0",[],["rdx"],NONE);
                                                                emitO("idivq %'s2, %rax",["rax","rdx",munchExp e2],["rax","rdx"],NONE);
                                                                emitM("movq %'s0, %'d0","rax",r)))
      | munchExp (BINOP (DIV, e1, e2)) = result ( fn r => (emitM("movq %'s0, %'d0",munchExp e1,"rax");
                                                           emitO("movq $0, %'d0",[],["rdx"],NONE);
                                                           emitO("idivq %'s2, %rax",["rax","rdx",munchExp e2],["rax","rdx"],NONE);
                                                           emitM("movq %'s0, %'d0","rax",r)))
      | munchExp (MEM e) = result (fn r => emitM("movq (%'s0), %'d0",munchExp e,r))
	  | munchExp (CALL f) = result (fn r => let val _ = munchStm(EXP(CALL f)) in emitM("movq %'s0, %'d0",rv,r) end)
      | munchExp (ESEQ _) = raise Fail "ESEQ no deberia aparecer aqui."
      | munchExp exp = raise Fail "Casos no cubiertos en tigercodegen.munchExp"

	  (* munchCall: Tree.exp list -> unit
	   * Push all the argument to the registers and the stack according to the
	   * calling convention. *)
     and munchCall n argss =
		let (* Saco los argumentos de la pila para restaurar el estado de la misma *)
            val pop_list = ref ([]:instr list)
            fun getLen xs = Int.toString(List.length xs)
            fun emitP (ass,dst)  = (pop_list := (IOPER{assem=ass,src=[],dst=dst,jump=NONE}::(!pop_list)))
            fun emit_pops []     = ()
              | emit_pops (x::xs) = (emit(x);emit_pops(xs))
            (*Mueve los argumentos a la pila*)
            fun args2stack [] = []
              | args2stack (x::args) = (emitO("pushq %'d0 # Pusheo "^(getLen args),[],[munchExp x],NONE);
                                        emitP("popq %'d0 # Popeo "^(getLen args),[result (fn r => munchExp(TEMP r))]);
                                        args2stack(args))
            (*Mueve los argumentos a los registros*)
            fun args2regs [] _ = [](*Finaliza la funcion*)
              | args2regs args [] = args2stack(rev(args))(*ya use los 6 regitros para argumentos*)
              | args2regs (x::args) (r::regs) = let val _ = munchStm(MOVE (TEMP r, x)) in r ::(args2regs args regs) end
            val src_reg = args2regs argss argsregs
        in emitO("xorq %'d0, %'d0 #cnt argumentos de punto flotante",[],[rv],NONE);emitO("call "^n,src_reg,callersaves,NONE) end

in munchStm stm; rev(!ilist) end

end
