structure tigertrans :> tigertrans = struct

open tigerframe
open tigertree
open tigertemp
open tigerabs

exception breakexc
exception divCero
exception OrphanBreak
	
type level = {parent:frame option , frame: frame, level: int}
type access = tigerframe.access

type frag = tigerframe.frag
val fraglist = ref ([]: frag list)

val actualLevel = ref ~1 (* _tigermain debe tener level = 0. *)
fun getActualLev() = !actualLevel
fun getlevel {parent, frame, level} = level

val outermost: level = {parent=NONE,
	frame=newFrame{name="_tigermain", formals=[]}, level=getActualLev()}
fun newLevel{parent={parent, frame, level}, name, formals} =
	{
	parent=SOME frame,
	frame=newFrame{name=name, formals=formals},
	level=level+1}
fun allocArg{parent, frame, level} b = tigerframe.allocArg frame b
fun allocLocal{parent, frame, level} b = tigerframe.allocLocal frame b
fun formals{parent, frame, level} = tigerframe.formals frame

datatype exp =
	Ex of tigertree.exp
	| Nx of tigertree.stm
	| Cx of label * label -> tigertree.stm

fun seq [] = EXP (CONST 0)
	| seq [s] = s
	| seq (x::xs) = SEQ (x, seq xs)

fun unEx (Ex e) = e
	| unEx (Nx s) = ESEQ(s, CONST 0)
	| unEx (Cx cf) =
	let
		val r = newtemp()
		val t = newlabel()
		val f = newlabel()
	in
		ESEQ(seq [MOVE(TEMP r, CONST 1),
			cf (t, f),
			LABEL f,
			MOVE(TEMP r, CONST 0),
			LABEL t],
			TEMP r)
	end

fun unNx (Ex e) = EXP e
	| unNx (Nx s) = s
	| unNx (Cx cf) =
	let
		val t = newlabel()
		val f = newlabel()
	in
		seq [cf(t,f),
			LABEL t,
			LABEL f]
	end

fun unCx (Nx s) = raise Fail ("Error (UnCx(Nx..))")
	| unCx (Cx cf) = cf
	| unCx (Ex (CONST 0)) =
	(fn (t,f) => JUMP(NAME f, [f]))
	| unCx (Ex (CONST _)) =
	(fn (t,f) => JUMP(NAME t, [t]))
	| unCx (Ex e) =
	(fn (t,f) => CJUMP(NE, e, CONST 0, t, f))

fun Ir(e) =
	let	fun aux(Ex e) = tigerit.tree(EXP e)
		| aux(Nx s) = tigerit.tree(s)
		| aux _ = raise Fail "bueno, a completar!"
		fun aux2(PROC{body, frame}) = aux(Nx body)
		| aux2(STRING(l, "")) = l^":\n"
		| aux2(STRING("", s)) = "\t"^s^"\n"
		| aux2(STRING(l, s)) = l^":\t"^s^"\n"
		fun aux3 [] = ""
		| aux3(h::t) = (aux2 h)^(aux3 t)
	in	aux3 e end
fun nombreFrame frame = print(".globl " ^ tigerframe.name frame ^ "\n")

(* While y for necesitan la u'ltima etiqueta para un break *)
local
	val salidas: label option tigerpila.Pila = tigerpila.nuevaPila1 NONE
in
	val pushSalida = tigerpila.pushPila salidas
	fun popSalida() = tigerpila.popPila salidas
	fun lenSalida() = tigerpila.lenPila salidas
	fun topSalida() =
		case tigerpila.topPila salidas of
			SOME l => l
			| NONE => raise OrphanBreak			
end

val datosGlobs = ref ([]: frag list)
fun procEntryExit{level: level, body} =
	let	val label = STRING(name(#frame level), "")
		val body' = PROC{frame= #frame level, body=unNx body}
		val final = STRING(";;-------", "")
	in	datosGlobs:=(!datosGlobs@[body']) end
fun getResult() = !datosGlobs

fun stringLen s =
	let	fun aux[] = 0
		| aux(#"\\":: #"x"::_::_::t) = 1+aux(t)
		| aux(_::t) = 1+aux(t)
	in	aux(explode s) end

fun stringExp(s: string) =
	let	val l = newlabel()
		val len = ".long "^makestring(stringLen s)
		val str = ".string \""^s^"\""
		val _ = datosGlobs:=(!datosGlobs @ [STRING(l, len), STRING("", str)])
	in	Ex(NAME l) end
fun preFunctionDec() =
	(pushSalida(NONE);
	actualLevel := !actualLevel+1)

fun functionDec(e, l, proc) =
	let	val body =
				if proc then unNx e
				else MOVE(TEMP rv, unEx e)
		val body' = procEntryExit1(#frame l, body)
		val () = procEntryExit{body=Nx body', level=l}
	in	Ex(CONST 0) end

fun postFunctionDec() =
	(popSalida(); actualLevel := !actualLevel-1)

fun unitExp() = Ex (CONST 0)

fun nilExp() = Ex (CONST 0)

fun intExp i = Ex (CONST i)

fun simpleVar(InFrame offset, nivel) = (*COMPLETADO*)
  (*x: nivel final, y: nivel actual*)
	let fun staticLink 0 = TEMP fp
			| staticLink n = (case n > 0 of
													true => MEM(BINOP(PLUS,staticLink (n-1),CONST fpPrevLev))
													| false => raise Fail "Error interno 0 - tigertrans.sml")
			val instr = MEM(BINOP(PLUS,staticLink (getActualLev()-nivel),CONST offset))
	in Ex(instr) end
| simpleVar(InReg tmp, nivel) = Ex(TEMP tmp)

fun varDec(acc) = simpleVar(acc, getActualLev())

fun fieldVar(var, field) = (*COMPLETADO*)
	let val tmp = newtemp()
			val var' = unEx var
			val instr = [EXP(externalCall("_checknil", [var'])),
									MOVE(TEMP tmp, MEM(BINOP(PLUS,var',BINOP(MUL,CONST field,CONST wSz))))]
	in Ex(ESEQ(seq instr,TEMP tmp)) end

fun subscriptVar(arr, ind) =
let
	val a = unEx arr
	val i = unEx ind
	val ra = newtemp()
	val ri = newtemp()
in
	Ex( ESEQ(seq[MOVE(TEMP ra, a),
		MOVE(TEMP ri, i),
		EXP(externalCall("_checkindex", [TEMP ra, TEMP ri]))],
		MEM(BINOP(PLUS, TEMP ra,
			BINOP(MUL, TEMP ri, CONST tigerframe.wSz)))))
end

fun recordExp l = (*COMPLETADO*)
	let fun cmp ((_,nx,_),(_,ny,_)) = Int.compare (nx,ny)
			val campos = map (fn (exp,n) => let val tmp = TEMP (newtemp()) in (MOVE(tmp,unEx exp),n,tmp) end) l
			val camposreg = map (#3) campos
			val campos = map (fn (e,_,_) => e) (Listsort.sort cmp campos)
	in Ex(ESEQ(seq campos,externalCall("_allocRecord",camposreg))) end

fun arrayExp{size, init} =
let
	val s = unEx size
	val i = unEx init
in
	Ex (externalCall("_allocArray", [s, i]))
end

fun callExp (name,external,isproc,lev:level,args) = (*COMPLETADO*)
	(*Cambiamos el nombre de la funcion en tigerseman, eso no me rompe todo?*)
	let val args' = map (fn exp => let val tmp = TEMP (newtemp()) in (tmp,MOVE(tmp,unEx exp)) end) args
			val t = TEMP (newtemp())
			val argtmp = map (#1) args'(*No deberian salir de aca la access list?,Por que todos a regitros?, Varialbes escapadas??*)
			val argins = map (#2) args'
			val tmpsl = TEMP (newtemp())
			val inssl = MOVE(tmpsl,
									case Int.compare(getActualLev(),getlevel lev) of
										LESS => TEMP fp
										| EQUAL => MEM(BINOP(PLUS,TEMP fp,CONST (2*wSz)))
										| GREATER => 
											let val tmp = TEMP (newtemp())
													fun recorre 0 = []
													|   recorre n = MOVE(tmp,MEM(BINOP(PLUS,tmp,CONST (2*wSz))))::recorre (n-1)
											in ESEQ(seq ((MOVE(tmp,TEMP fp))::(recorre (getActualLev()-getlevel lev))),tmp) end
									)
	in Ex(ESEQ(seq (argins@[inssl]@[EXP (CALL (NAME name,tmpsl::argtmp)),MOVE (t,TEMP rv)]),t)) end

fun letExp ([], body) = Ex (unEx body)
 |  letExp (inits, body) = Ex (ESEQ(seq inits,unEx body))

fun breakExp() = (*COMPLETADO*)
	let val l = topSalida()
	in Nx(JUMP(NAME l,[l])) end

fun seqExp ([]:exp list) = Nx (EXP(CONST 0))
	| seqExp (exps:exp list) =
		let
			fun unx [e] = []
				| unx (s::ss) = (unNx s)::(unx ss)
				| unx[] = []
		in
			case List.last exps of
				Nx s =>
					let val unexps = map unNx exps
					in Nx (seq unexps) end
				| Ex e => Ex (ESEQ(seq(unx exps), e))
				| cond => Ex (ESEQ(seq(unx exps), unEx cond))
		end

fun preWhileForExp() = pushSalida(SOME(newlabel()))

fun postWhileForExp() = (popSalida(); ())

fun whileExp {test: exp, body: exp, lev:level} =
let
	val cf = unCx test
	val expb = unNx body
	val (l1, l2, l3) = (newlabel(), newlabel(), topSalida())
in
	Nx (seq[LABEL l1,
		cf(l2,l3),
		LABEL l2,
		expb,
		JUMP(NAME l1, [l1]),
		LABEL l3])
end

fun forExp {lo, hi, var, body} = (*COMPLETADO*)
	let val var' = unEx var
			val lo' = unEx lo
			val hi' = unEx hi
			val body' = unNx body
			val (lsigue,lfin) = (newlabel(),topSalida())
			val instr = [MOVE(var',lo'),
									CJUMP(GE,var',hi',lfin,lsigue),
									LABEL lsigue,
									body',
									MOVE(var',BINOP(PLUS,var',CONST 1)),
									CJUMP(GE,var',hi',lfin,lsigue),
									LABEL lfin]
	in Nx(seq instr) end

fun ifThenExp{test, then'} = (*COMPLETADO*)
	let val test' = unCx test
			val th' = unNx then'
			val (ltrue,lfalse) = (newlabel(),newlabel())
			val instr = [test'(ltrue,lfalse),
									LABEL ltrue,
									th',
									LABEL lfalse]
	in Nx(seq instr) end

fun ifThenElseExp {test,then',else'} = (*COMPLETADO*)
	let val test' = unCx test
			val th' = unEx then'
			val el' = unEx else'
			val tmp = newtemp()
			val (ltrue,lfalse,lsigue) = (newlabel(),newlabel(),newlabel())
			val instr = [test'(ltrue,lfalse),
									LABEL ltrue,
									MOVE(TEMP tmp,th'),
									JUMP(NAME lsigue,[lsigue]),
									LABEL lfalse,
									MOVE(TEMP tmp,el'),
									LABEL lsigue]
	in Ex(ESEQ(seq instr,TEMP tmp)) end

fun ifThenElseExpUnit {test,then',else'} = (*COMPLETADO*)
	let val test' = unCx test
			val th' = unNx then'
			val el' = unNx else'
			val (ltrue,lfalse,lsigue) = (newlabel(),newlabel(),newlabel())
			val instr = [test'(ltrue,lfalse),
									LABEL ltrue,
									th',
									JUMP(NAME lsigue,[lsigue]),
									LABEL lfalse,
									el',
									LABEL lsigue]
	in Nx(seq instr) end

fun assignExp{var, exp} =
let
	val v = unEx var
	val vl = unEx exp
in
	Nx (MOVE(v,vl))
end

fun binOpIntExp {left, oper=PlusOp, right} = Ex(BINOP(PLUS,unEx left,unEx right)) (*COMPLETADO*)
	| binOpIntExp {left, oper=MinusOp, right} = Ex(BINOP(MINUS,unEx left,unEx right))
	| binOpIntExp {left, oper=TimesOp, right} = Ex(BINOP(MUL,unEx left,unEx right))
	| binOpIntExp {left, oper=DivideOp, right} = Ex(BINOP(DIV,unEx left,unEx right))
	| binOpIntExp {left, oper, right} = raise Fail "Error interno 1 - tigertrans.sml"

fun binOpIntRelExp {left,oper=LtOp,right} = Cx(fn (lt,lf) => CJUMP(LT,unEx left,unEx right,lt,lf)) (*COMPLETADO*)
	| binOpIntRelExp {left,oper=LeOp,right} = Cx(fn (lt,lf) => CJUMP(LE,unEx left,unEx right,lt,lf))
	| binOpIntRelExp {left,oper=GtOp,right} = Cx(fn (lt,lf) => CJUMP(GT,unEx left,unEx right,lt,lf))
	| binOpIntRelExp {left,oper=GeOp,right} = Cx(fn (lt,lf) => CJUMP(GE,unEx left,unEx right,lt,lf))
	| binOpIntRelExp {left,oper,right} = raise Fail "Error interno 2 - tigertrans.sml"

fun binOpStrExp {left,oper=LtOp,right} = Cx(fn (lt,lf) => CJUMP(LT,externalCall("_stringCompare", [unEx left, unEx right]),CONST 0,lt,lf)) (*COMPLETADO*)
	| binOpStrExp {left,oper=LeOp,right} = Cx(fn (lt,lf) => CJUMP(LE,externalCall("_stringCompare", [unEx left, unEx right]),CONST 0,lt,lf))
	| binOpStrExp {left,oper=GtOp,right} = Cx(fn (lt,lf) => CJUMP(GT,externalCall("_stringCompare", [unEx left, unEx right]),CONST 0,lt,lf))
	| binOpStrExp {left,oper=GeOp,right} = Cx(fn (lt,lf) => CJUMP(GE,externalCall("_stringCompare", [unEx left, unEx right]),CONST 0,lt,lf))
	| binOpStrExp {left,oper,right} = raise Fail "Error interno 3 - tigertrans.sml"

end
