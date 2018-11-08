structure tigerseman :> tigerseman =
struct

open tigerabs
open tigersres
open tigertrans

type expty = {exp: unit, ty: Tipo}

type venv = (string, EnvEntry) tigertab.Tabla
type tenv = (string, Tipo) tigertab.Tabla

val tab_tipos : (string, Tipo) Tabla = tabInserList(
	tabNueva(),
	[("int", TInt), ("string", TString)])

val levelPila: tigertrans.level tigerpila.Pila = tigerpila.nuevaPila1(tigertrans.outermost) 
fun pushLevel l = tigerpila.pushPila levelPila l
fun popLevel() = tigerpila.popPila levelPila 
fun topLevel() = tigerpila.topPila levelPila

val tab_vars : (string, EnvEntry) Tabla = tabInserList(
	tabNueva(),
	[("print", Func{level=topLevel(), label="print",
		formals=[TString], result=TUnit, extern=true}),
	("flush", Func{level=topLevel(), label="flush",
		formals=[], result=TUnit, extern=true}),
	("getchar", Func{level=topLevel(), label="getstr",
		formals=[], result=TString, extern=true}),
	("ord", Func{level=topLevel(), label="ord",
		formals=[TString], result=TInt, extern=true}),
	("chr", Func{level=topLevel(), label="chr",
		formals=[TInt], result=TString, extern=true}),
	("size", Func{level=topLevel(), label="size",
		formals=[TString], result=TInt, extern=true}),
	("substring", Func{level=topLevel(), label="substring",
		formals=[TString, TInt, TInt], result=TString, extern=true}),
	("concat", Func{level=topLevel(), label="concat",
		formals=[TString, TString], result=TString, extern=true}),
	("not", Func{level=topLevel(), label="not",
		formals=[TInt], result=TInt, extern=true}),
	("exit", Func{level=topLevel(), label="exit",
		formals=[TInt], result=TUnit, extern=true})
	])

fun tipoReal t = t

fun tiposIguales (TRecord _) TNil = true
	  | tiposIguales TNil (TRecord _) = true 
  | tiposIguales (TRecord (_, u1)) (TRecord (_, u2 )) = (u1=u2)
  | tiposIguales (TArray (_, u1)) (TArray (_, u2)) = (u1=u2)
  | tiposIguales (TTipo _) a = raise Fail "No debería pasar! (1)"
  | tiposIguales a (TTipo _) = raise Fail "No debería pasar! (2)"
  | tiposIguales a b = (a=b)

fun transExp(venv, tenv) =
	let fun error(s, p) = raise Fail ("Error -- línea "^Int.toString(p)^": "^s^"\n")
		fun trexp(VarExp v)      =           trvar(v)
		| trexp(UnitExp _)       =           {exp=unitExp(), ty=TUnit}
		| trexp(NilExp _)        =           {exp=nilExp(), ty=TNil}
		| trexp(IntExp(i, _))    =           {exp=intExp i, ty=TInt}
		| trexp(StringExp(s, _)) =           {exp=stringExp(s), ty=TString}
		| trexp(CallExp({func, args}, nl)) =(*COMPLETADO*)
			let
				val {level,label,formals,result,extern} = 
					case tabBusca(func,venv) of
						SOME (Func t) => t
						| SOME _      => error("El nombre ("^func^") esta definido como una variable",nl)
						| _           => error("Funcion ("^func^") no definida",nl)
				val (tArgs,eArgs) = foldr (fn (a,(lt,le)) => let val {exp=expArg, ty=tipoArg} = trexp(a) in (tipoArg::lt,expArg::le) end) ([],[]) args
				fun comparar ([],[])           = ()
					| comparar ([],_)            = error("No hay suficientes argumentos para la funcion ("^func^")",nl)
					| comparar (_,[])            = error("Hay demasiados argumentos para la funcion ("^func^")",nl)
					| comparar ((x::xs),(y::ys)) = if tiposIguales x y then comparar(xs,ys)
																				 else error("Los tipos de los argumentos de la funcion no coinciden"^
																										" con los tipos de los argumentos pasados",nl)
				val _ = comparar(tArgs,formals)
			in {exp=callExp (func,extern,result <> TUnit,level,eArgs), ty=result} end
		| trexp(OpExp({left, oper=EqOp, right}, nl)) =
			let
				val {exp=expl, ty=tyl} = trexp left
				val {exp=expr, ty=tyr} = trexp right
			in
				if tiposIguales tyl tyr andalso not (tyl=TNil andalso tyr=TNil) andalso tyl<>TUnit then 
					{exp=if tiposIguales tyl TString then binOpStrExp {left=expl,oper=EqOp,right=expr}
							 else binOpIntRelExp {left=expl,oper=EqOp,right=expr}, ty=TInt}
				else error("Tipos no comparables", nl)
			end
		| trexp(OpExp({left, oper=NeqOp, right}, nl)) = 
			let
				val {exp=expl, ty=tyl} = trexp left
				val {exp=expr, ty=tyr} = trexp right
			in
				if tiposIguales tyl tyr andalso not (tyl=TNil andalso tyr=TNil) andalso tyl<>TUnit then 
					{exp=if tiposIguales tyl TString then binOpStrExp {left=expl,oper=NeqOp,right=expr}
							 else binOpIntRelExp {left=expl,oper=NeqOp,right=expr}, ty=TInt}
				else error("Tipos no comparables", nl)
			end
		| trexp(OpExp({left, oper, right}, nl)) = 
			let
				val {exp=expl, ty=tyl} = trexp left
				val {exp=expr, ty=tyr} = trexp right
			in
				if tiposIguales tyl tyr then
					case oper of
						PlusOp 		 => if tipoReal tyl=TInt then {exp=binOpIntExp {left=expl, oper=oper, right=expr},ty=TInt}
													else error("Error de tipos", nl)
						| MinusOp  => if tipoReal tyl=TInt then {exp=binOpIntExp {left=expl, oper=oper, right=expr},ty=TInt}
													else error("Error de tipos", nl)
						| TimesOp  => if tipoReal tyl=TInt then {exp=binOpIntExp {left=expl, oper=oper, right=expr},ty=TInt}
													else error("Error de tipos", nl)
						| DivideOp => if tipoReal tyl=TInt then {exp=binOpIntExp {left=expl, oper=oper, right=expr},ty=TInt}
													else error("Error de tipos", nl)
						| LtOp 		 => if tipoReal tyl=TInt orelse tipoReal tyl=TString then {exp=if tipoReal tyl=TInt
																																										 then binOpIntRelExp {left=expl,oper=oper,right=expr}
																																				  					 else binOpStrExp {left=expl,oper=oper,right=expr},ty=TInt} 
													else error("Error de tipos", nl)
						| LeOp 		 => if tipoReal tyl=TInt orelse tipoReal tyl=TString then {exp=if tipoReal tyl=TInt
																																										 then binOpIntRelExp {left=expl,oper=oper,right=expr}
																																										 else binOpStrExp {left=expl,oper=oper,right=expr},ty=TInt} 
													else error("Error de tipos", nl)
						| GtOp 		 => if tipoReal tyl=TInt orelse tipoReal tyl=TString then {exp=if tipoReal tyl=TInt
																																										 then binOpIntRelExp {left=expl,oper=oper,right=expr}
																																										 else binOpStrExp {left=expl,oper=oper,right=expr},ty=TInt} 
													else error("Error de tipos", nl)
						| GeOp 		 => if tipoReal tyl=TInt orelse tipoReal tyl=TString then {exp=if tipoReal tyl=TInt
																																										 then binOpIntRelExp {left=expl,oper=oper,right=expr}
																																										 else binOpStrExp {left=expl,oper=oper,right=expr},ty=TInt} 
													else error("Error de tipos", nl)
						| _ 			 => raise Fail "No debería pasar! (3)"
				else error("Error de tipos", nl)
			end
		| trexp(RecordExp({fields, typ}, nl)) =
			let
				fun zip [] _ = []
				| zip (x::xs) n = (x,n):: (zip xs (n+1))
				(* Traducir cada expresión de fields *)
				val tfields' = zip(map (fn (sy,ex) => (sy, trexp ex)) fields) 0
				fun cmp (((sx,_),_),((sy,_),_)) = String.compare (sx,sy)
				val tfields = Listsort.sort cmp tfields'
				(* Buscar el tipo *)
				val (tyr, cs) = case tabBusca(typ, tenv) of
													SOME t => (case tipoReal t of
																			TRecord (cs, u) => (TRecord (cs, u), cs)
																			| _             => error(typ^" no es de tipo record", nl))
													| NONE => error("Tipo inexistente ("^typ^")", nl)
				(* Verificar que cada campo esté en orden y tenga una expresión del tipo que corresponde *)
				fun verificar [] [] = []
				  | verificar (c::cs) [] = error("Faltan campos", nl)
				  | verificar [] (c::cs) = error("Sobran campos", nl)
				  | verificar ((s,ref t,_)::cs) (((sy,{exp,ty}),n)::ds) =
						if s<>sy then error("Error de campo", nl)
						else if tiposIguales ty t then (exp, n)::(verificar cs ds)
							 else error("Error de tipo del campo "^s, nl)
				val lf = (verificar cs tfields)
			in
				{exp=recordExp lf, ty=tyr}
			end
		| trexp(SeqExp(s, nl)) =
			let	val lexti = map trexp s
					val exprs = map (fn{exp, ty} => exp) lexti
					val {exp, ty=tipo} = hd(rev lexti)
			in	{ exp=seqExp (exprs), ty=tipo } end
		| trexp(AssignExp({var=SimpleVar s, exp}, nl)) = (*COMPLETADO*)
			let
				val {ty=tys,...} = case tabBusca(s,venv) of
												SOME (Var a)      => a
												| SOME (VIntro _) => error("No es posible asignar valores a ("^s^") ",nl)
												| SOME _  		    => error("El nombre ("^s^") esta definido como una funcion",nl)
												| _ 			 	      => error("Variable ("^s^") no definida",nl)
				val {exp=expexp,ty=tyexp} = trexp exp
				val {exp=expvar,ty} = trvar (SimpleVar s,nl)
				val _ = if tiposIguales tyexp tys then () 
								else error("Intento de asignar un valor del tipo incorrecto a la variable ("^s^")",nl)
			in {exp=assignExp{var=expvar,exp=expexp}, ty=TUnit} end			
		| trexp(AssignExp({var=FieldVar (v,s), exp}, nl)) = (*COMPLETADO*)
			let
				val {exp=expvar,ty=tyvar} = trvar (FieldVar (v,s),nl)
				val {exp=expexp,ty=tyexp} = trexp exp
				val _ = if tiposIguales tyvar tyexp then ()
								else error("Intentando asignar al record un tipo distinto",nl)
			in {exp=assignExp{var=expvar,exp=expexp}, ty=TUnit} end
		| trexp(AssignExp({var=SubscriptVar (v,e), exp}, nl)) = (*COMPLETADO*)
			let
				val {exp=expvar,ty=tyvar} = trvar (SubscriptVar (v,e),nl)
				val {exp=expexp,ty=tyexp} = trexp exp
				val _ = if tiposIguales tyvar tyexp then ()
								else error("Intentando asignar al arreglo un tipo distinto",nl)
			in {exp=assignExp{var=expvar,exp=expexp}, ty=TUnit} end
		| trexp(IfExp({test, then', else'=SOME else'}, nl)) =
			let val {exp=testexp, ty=tytest} = trexp test
			    val {exp=thenexp, ty=tythen} = trexp then'
			    val {exp=elseexp, ty=tyelse} = trexp else'
			in
				if tipoReal tytest=TInt andalso tiposIguales tythen tyelse then {exp=if tipoReal tythen=TUnit 
																																						 then ifThenElseExpUnit {test=testexp,then'=thenexp,else'=elseexp} 
																																						 else ifThenElseExp {test=testexp,then'=thenexp,else'=elseexp}, ty=tythen}
				else error("Los tipos del then y else del IF no coinciden" ,nl)
			end
		| trexp(IfExp({test, then', else'=NONE}, nl)) =
			let val {exp=exptest,ty=tytest} = trexp test
			    val {exp=expthen,ty=tythen} = trexp then'
			in
				if tipoReal tytest=TInt andalso tythen=TUnit then	{exp=ifThenExp{test=exptest, then'=expthen}, ty=TUnit}
				else error("El then de un IF sin else debe tener tipo TUnit", nl)
			end
		| trexp(WhileExp({test, body}, nl)) =
			let	
				val ttest = trexp test
				val _ = preWhileForExp()
				val {exp = expbody, ty = tbody} = trexp body
				val expwhile =  if tipoReal (#ty ttest) = TInt andalso tbody = TUnit 
												then {exp=whileExp {test=(#exp ttest), body=expbody, lev=topLevel()}, ty=TUnit}
												else if tipoReal (#ty ttest) <> TInt then error("Error de tipo en la condición", nl)
												else error("El cuerpo de un while no puede devolver un valor", nl)			
				val _ = postWhileForExp()			
			in expwhile	end
		| trexp(ForExp({var, escape, lo, hi, body}, nl)) =(*COMPLETADO*)
			let
				val {exp = explo, ty = tlo} = trexp lo
				val {exp = exphi, ty = thi} = trexp hi
				val _ = if tlo = TInt andalso thi=TInt then ()
								else error("Los indices del for deben ser enteros",nl)
				val varAccess = allocLocal (topLevel()) (!escape)
				val varLevel = getActualLev()
				val venv' = tabRInserta(var,VIntro{access= varAccess,level= varLevel},venv)
				val expvar = simpleVar (varAccess,varLevel)								
				val _ = preWhileForExp()
				val {exp = expbody, ty = tbody} = transExp (venv',tenv) body
				val expfor = {exp=forExp{lo=explo, hi=exphi ,var=expvar,body=expbody}, ty=TUnit}				
				val _ = postWhileForExp()
				val _ = if tbody = TUnit then ()
								else error("El tipo del cuerpo del for debe ser TUnit",nl)
			in expfor end
		| trexp(LetExp({decs, body}, _)) =
			let
				fun aux (d, (v, t, exps1)) =
					let val (v', t', exps2) = trdec (v, t) d
					in (v', t', exps1@exps2) end
				val (venv', tenv', expdecs) = List.foldl aux (venv, tenv, []) decs
				val {exp=expbody,ty=tybody}=transExp (venv', tenv') body
			in {exp=seqExp(expdecs@[expbody]), ty=tybody} end
		| trexp(BreakExp nl) = let 
															val expbreak = breakExp()
																						 handle tigertrans.OrphanBreak => error("Se encontro un break fuera de un ciclo ",nl)
													 in {exp=expbreak, ty=TUnit} end (*COMPLETADO*)
		| trexp(ArrayExp({typ, size, init}, nl)) = (*COMPLETADO*)
			let val t = case tabBusca(typ,tenv) of
										SOME a => a
										| NONE => error("El tipo ("^typ^") no esta definido",nl)
				val t' = case t of
									TArray (ref u,_) => u
									| _ 						 => error("El tipo ("^typ^") no es un array",nl)
				val {exp=expsize, ty=tysize} = trexp size
				val _ = if tysize = TInt then ()
								else error("El tamaño del array debe ser un entero",nl)
				val {exp=expinit, ty=tyinit} = trexp init
				val _ = if tiposIguales tyinit t' then ()
								else error("El tipo del valor inicial no coincide con el tipo del array",nl)
			in {exp=arrayExp{size=expsize, init=expinit}, ty=t} end
		and trvar(SimpleVar s, nl) =(*COMPLETADO*)
			let
				val {ty=tvar,access, level} = case tabBusca(s,venv) of
										SOME (Var a)											=> a
										| SOME (VIntro {access,level})  	=> {ty=TInt, access=access, level=level}
										| _ 													    => error("Variable ("^s^") no definida",nl)
			in {exp=simpleVar(access,level), ty=tvar} end
		| trvar(FieldVar(v, s), nl) = (*COMPLETADO*)
			let
				val {exp=expv,ty=tyv} = trvar(v,nl)
				val l = case tyv of
									TRecord (ls,_) => ls
									| _ 					 => error("El tipo de la variable no es un record",nl)
				val (tiporesult,intresult) = case List.find (fn((name,_,_)) => name = s) l of
																			SOME (_,ref tipo,n) => (tipo,n)
																			| NONE 							=> error("El record no posee un campo llamado ("^s^")",nl)
			in {exp=fieldVar(expv,intresult), ty=tiporesult} end
		| trvar(SubscriptVar(v, e), nl) = (*COMPLETADO*)
			let
				val {exp=expv,ty=tyv} = trvar (v,nl)
				val t = case tyv of
									TArray (ref t',_) => t'
									| _ 							=> error("La variable no es de tipo TArray",nl)
				val {exp=expe,ty=tye} = trexp e
				val _ = if tye = TInt then ()
								else error("El indice del array debe ser un entero",nl)
			in {exp=subscriptVar(expv,expe), ty=t} end
		and trdec (venv, tenv) (VarDec ({name,escape,typ=NONE,init},pos)) = (*COMPLETADO*)
			let 
				val {exp=expinit,ty=tyinit} = transExp (venv,tenv) init
				val _ = case tyinit of
									TNil 		=> error("Se debe explicitar el tipo cuando se declara la variable ("^name^
																	 ") y se le quiere asignar el valor nil",pos)
									| TUnit => error("No se puede declarar la variable ("^name^") y asignarle algo del tipo Unit",pos)
									| _ 		=> ()
				val varAccess = allocLocal (topLevel()) (!escape)
				val venv' = tabInserta(name,Var {ty=tyinit,access= varAccess,level=getActualLev()},venv)
			in (venv', tenv, [assignExp{var = varDec varAccess,exp=expinit}]) end
		| trdec (venv,tenv) (VarDec ({name,escape,typ=SOME s,init},pos)) = (*COMPLETADO*)
			let
				val {exp=expinit,ty=tyinit} = transExp (venv,tenv) init
				val _ = if tiposIguales tyinit TUnit then error("No se puede declarar la variable ("^name^
																												") y asignarle algo del tipo TUnit",pos)
								else ()
				val t = case tabBusca(s,tenv) of
									SOME s' => if tiposIguales s' tyinit then s'
														 else error("El tipo de la declaracion de la variable ("^name^
																				") no coincide con el tipo del valor inicial asignado",pos)
									| NONE 	=> error("El tipo ("^s^") de la variable ("^name^") no esta definido",pos)
				val varAccess = allocLocal (topLevel()) (!escape)
				val venv' = tabInserta(name,Var {ty=t,access= varAccess,level=getActualLev()},venv)(*TabRInserta?*)
			in (venv', tenv, [assignExp{var = varDec varAccess,exp=expinit}]) end
		| trdec (venv,tenv) (FunctionDec fs) =(*COMPLETADO*)
			let
				val _ = preFunctionDec()				
				(*Chequear que no se repitan los nombres de las funciones en el mismo batch*)
				val _ = List.foldl (fn (({name,...},pos),lb) =>
										if List.exists (fn x => x=name) lb then error("La funcion ("^name^") ya esta"^
																																	" definida en el mismo batch",pos) 
										else name::lb) [] fs
				(*Chequear que no se repitan los nombres de los argumentos en una misma funcion*)
				val _ = List.map (fn ({name=fname,params,...},pos) =>
										List.foldl (fn (({name=pname,...}),lb) => 
														if List.exists (fn x => x=pname) lb then error("El parametro ("^pname^") esta repetido en"^
																																					 " la definicion de ("^fname^")",pos) 
														else pname::lb) [] params) fs
				(*(*Chequear los tipos existan y sean iguales entre result y body*)
				val tf = List.map (fn ({name=fname,params,result,body},pos) =>
									let	
										fun checkpar ps = List.map (fn ({name=pname,typ,escape}) =>
															case typ of
																NameTy s => (case tabBusca(s,tenv) of
																							SOME t' => (pname,t',escape)
																							| _    	=>  error("El tipo del parametro ("^pname^") de la funcion ("
																																^fname^") no esta definido",pos))
																| _ 		 => error("Error interno relacionado al parser",pos)) ps
										val ps = checkpar params(*Chequeo que los tipos de los parametros esten bien*)
										val vformals = List.map (fn (name,tipo,escape) => tipo) ps
										fun fresult res bod =
											let 
												val tresult = case res of
																				SOME s => (case tabBusca(s,tenv) of
																										SOME t' => t'
																										| NONE  => error("El tipo ("^s^") no esta definido",pos))
																				| NONE => TUnit
											in tresult end
										val vresult = fresult result body
									in (fname,{level=topLevel(),label=fname,formals=vformals,result=vresult,extern=false},ps,body,pos) end) fs
				
				val lf = List.map (fn (name,{level,...},ps,body,pos) => print ("Defino "^name^" con nivel "^Int.toString(getlevel level)^"\n")) tf
				val lf = List.map (fn (name,fnc,ps,body,pos) => (name,Func fnc)) tf
				val venv' = tabInserList(venv,lf)
				val explist = List.map (fn (name,{result,level,...},ps,body,pos) => 
												let
													val ps' = List.map (fn (name,tipo,escape) => (name,Var {ty=tipo,access=allocLocal (topLevel()) (!escape),level=getActualLev()})) ps
													val boolformals = map (fn (_,_,	escape) => !escape) ps
													val acclist = List.map (fn (_,Var {access,...}) => access
																										 | _ => error("Error interno ralacionado a accesslist",pos)) ps'
													val nlevel = newLevel ({parent=level,name=name,formals=boolformals,accesslist=acclist})
													val _ = pushLevel(nlevel)
													val venv_intern = tabInserList(venv',ps')
													val {exp=expbody,ty=tbody} = transExp (venv_intern,tenv) body
													val _ = if tiposIguales result tbody then () else error("La funcion ("^name^") no posee"^
																																						" el mismo tipo que su cuerpo",pos)
													val _ = popLevel()												
												in functionDec (expbody,nlevel,result=TUnit) end) tf
				*)
					(*Veo que los tipo existan*)
					val typfuns = List.map (fn ({name=fname,params,result,body},pos) =>(*Veo los tipos de los argumentos y el resultado*)
									let	
										fun checkpar par = List.map (fn ({name=pname,typ,escape}) =>
															case typ of
																NameTy s => (case tabBusca(s,tenv) of
																							SOME t' => (pname,t',escape)
																							| _    	=>  error("El tipo del parametro ("^pname^") de la funcion ("
																																^fname^") no esta definido",pos))
																| _ 		 => error("Error interno relacionado al parser",pos)) par
										val ps = checkpar params(*Chequeo que los tipos de los parametros esten bien*)
										(*val tformals = List.map (fn (name,tipo,escape) => tipo) ps*)
										val tresult = case result of(*Chequeo el tipo del resultado*)
																		SOME s => (case tabBuscas(s,tenv) of
																									SOME t' => t'
																									|NONE   => error("El tipo ("^s^") no esta definido",pos))
																		|NONE  => TUnit
									in (fname,params,body,tresult,ps,pos) end) fs
									(*in (fname,{level=topLevel(),label=fname,formals=tformals,result=tresult,extern=false},ps,body,pos) end) fs*)
				val listfuns = List.map (fn (fname,params,body,tresult,ps,pos) => 
																	let 
																			val boolformals = List.map #3 ps
																			val acclist = List.map (allocLocal (topLevel()) (!escape)) ps																	
																			val fnc = {
																									label = fname,
																									formals = (List.map #2 ps),
																									result = tresult,
																									extern = false,
																									level = newLevel({parent=topLevel(),name=fname,formals=boolformals,accesslist=acclist})
																								}
																	in (fname,Func fnc) end) typfuns
				val explist = List.map () typfuns
				val _ = postFunctionDec()
			in (venv', tenv, explist) end
		| trdec (venv,tenv) (TypeDec ts) = (*COMPLETADO*)
			let
				(*TypeDec of ({name: symbol, ty: ty} * pos) list*)
				fun buscarep [] 											= (false,"",0) (*Toma un nombre y se fija si aparece en el resto de la lista*)
						| buscarep (({name=s,ty},pos)::t) = if List.exists (fn({name=x,...},p) => x=s) t then (true,s,pos) else buscarep t
				val reps = buscarep ts (*Si los nombre estan repetido, tiro el error correspondiente*)
				val _ = if #1 reps then error("El nombre de tipo ("^(#2 reps)^") esta repetido",#3 reps) else ()
				val ts' = List.map (#1) ts (*Me quedo con todos los tipos, descartando sus posiciones*)
				val tenv' = (tigertopsort.fijaTipos ts' tenv)(*Llama a fijarTipos, manejando las exepciones*)
							 					handle tigertopsort.Ciclo => error("Hay un ciclo en la declaracion de tipos", #2 (hd ts))
							 					| noExisteS ti 						=> error("El tipo ("^ti^") es inexistente", #2 (hd ts))
			in (venv, tenv', []) end 
	in trexp end
	
fun transProg ex =
	let	val main =
				LetExp({decs=[FunctionDec[({name="_tigermain", params=[],
								result=SOME ("int"), body=ex}, 0)]],
						body=UnitExp 0}, 0)
			val _ = transExp(tab_vars, tab_tipos) main
(*
			val {exp,ty} = transExp(tab_vars, tab_tipos) ex (*main*)
			val _ = case ty of
								TInt => ()
								| _  => raise Fail ("Error -- línea 0 : El programa no devuelve un entero\n")
*)
	in print "bien!\n" end
end

