structure tigerseman :> tigerseman =
struct

open tigerabs
open tigersres
open tigertrans

type expty = {exp: exp, ty: Tipo}

type venv = (string, EnvEntry) tigertab.Tabla
type tenv = (string, Tipo) tigertab.Tabla

val tab_tipos : (string, Tipo) Tabla = tabInserList(
	tabNueva(),
	[("int", TInt), ("string", TString)])

val tab_vars : (string, EnvEntry) Tabla = tabInserList(
	tabNueva(),
	[("print", Func{level=mainLevel, label="print",
		formals=[TString], result=TUnit, extern=true}),
	("flush", Func{level=mainLevel, label="flush",
		formals=[], result=TUnit, extern=true}),
	("getchar", Func{level=mainLevel, label="getstr",
		formals=[], result=TString, extern=true}),
	("ord", Func{level=mainLevel, label="ord",
		formals=[TString], result=TInt, extern=true}),
	("chr", Func{level=mainLevel, label="chr",
		formals=[TInt], result=TString, extern=true}),
	("size", Func{level=mainLevel, label="size",
		formals=[TString], result=TInt, extern=true}),
	("substring", Func{level=mainLevel, label="substring",
		formals=[TString, TInt, TInt], result=TString, extern=true}),
	("concat", Func{level=mainLevel, label="concat",
		formals=[TString, TString], result=TString, extern=true}),
	("not", Func{level=mainLevel, label="not",
		formals=[TInt], result=TInt, extern=true}),
	("exit", Func{level=mainLevel, label="exit",
		formals=[TInt], result=TUnit, extern=true})
	])

fun tipoReal t = t

fun tiposIguales (TRecord _) TNil = true
  | tiposIguales TNil (TRecord _) = true 
  | tiposIguales (TRecord (_, u1)) (TRecord (_, u2 )) = (u1=u2)
  | tiposIguales (TArray (_, u1)) (TArray (_, u2)) = (u1=u2)
  (*| tiposIguales (TTipo (_, r)) b =
		let
			val a = case !r of
				SOME t => t
				| NONE => raise Fail "No debería pasar! (1)"
		in
			tiposIguales a b
		end
  | tiposIguales a (TTipo (_, r)) =
		let
			val b = case !r of
				SOME t => t
				| NONE => raise Fail "No debería pasar! (2)"
		in
			tiposIguales a b
		end*)
  | tiposIguales (TTipo _) a = raise Fail "No debería pasar! (1)"
  | tiposIguales a (TTipo _) = raise Fail "No debería pasar! (2)"
  | tiposIguales a b = (a=b)

fun transExp(venv, tenv) =
	let fun error(s, p) = raise Fail ("Error -- línea "^Int.toString(p)^": "^s^"\n")
		fun trexp(VarExp v) = trvar(v)
		| trexp(UnitExp _) = {exp=SCAF, ty=TUnit}
		| trexp(NilExp _)= {exp=SCAF, ty=TNil}
		| trexp(IntExp(i, _)) = {exp=SCAF, ty=TInt}
		| trexp(StringExp(s, _)) = {exp=SCAF, ty=TString}
		| trexp(CallExp({func, args}, nl)) =(*COMPLETADO*)
			let
				val {level,label,formals,result,extern} = 
					case tabBusca(func,venv) of
						SOME (Func t) => t
						| SOME _ => error("El nombre ("^func^") esta definido como una variable",nl)
						| _ => error("Funcion ("^func^") no definida",nl)
				val tArgs = map (fn(a) => let val {exp, ty=tipoArg} = trexp(a) in tipoArg end) args
				fun comparar ([],[]) = ()
					| comparar ([],_) = error("No hay suficientes argumentos para la funcion ("^func^")",nl)
					| comparar (_,[]) = error("Hay demasiados argumentos para la funcion ("^func^")",nl)
					| comparar ((x::xs),(y::ys)) = if tiposIguales x y then comparar(xs,ys)
													else error("Los tipos de los argumentos de la funcion no coinciden con los tipos de los argumentos pasados",nl)
				val _ = comparar(tArgs,formals)
			in {exp=SCAF, ty=result} end
		| trexp(OpExp({left, oper=EqOp, right}, nl)) =
			let
				val {exp=_, ty=tyl} = trexp left
				val {exp=_, ty=tyr} = trexp right
			in
				if tiposIguales tyl tyr andalso not (tyl=TNil andalso tyr=TNil) andalso tyl<>TUnit then {exp=SCAF, ty=TInt}
					else error("Tipos no comparables", nl)
			end
		| trexp(OpExp({left, oper=NeqOp, right}, nl)) = 
			let
				val {exp=_, ty=tyl} = trexp left
				val {exp=_, ty=tyr} = trexp right
			in
				if tiposIguales tyl tyr andalso not (tyl=TNil andalso tyr=TNil) andalso tyl<>TUnit then {exp=SCAF, ty=TInt}
					else error("Tipos no comparables", nl)
			end
		| trexp(OpExp({left, oper, right}, nl)) = 
			let
				val {exp=_, ty=tyl} = trexp left
				val {exp=_, ty=tyr} = trexp right
			in
				if tiposIguales tyl tyr then
					case oper of
						PlusOp => if tipoReal tyl=TInt then {exp=SCAF,ty=TInt} else error("Error de tipos", nl)
						| MinusOp => if tipoReal tyl=TInt then {exp=SCAF,ty=TInt} else error("Error de tipos", nl)
						| TimesOp => if tipoReal tyl=TInt then {exp=SCAF,ty=TInt} else error("Error de tipos", nl)
						| DivideOp => if tipoReal tyl=TInt then {exp=SCAF,ty=TInt} else error("Error de tipos", nl)
						| LtOp => if tipoReal tyl=TInt orelse tipoReal tyl=TString then {exp=SCAF,ty=TInt} else error("Error de tipos", nl)
						| LeOp => if tipoReal tyl=TInt orelse tipoReal tyl=TString then {exp=SCAF,ty=TInt} else error("Error de tipos", nl)
						| GtOp => if tipoReal tyl=TInt orelse tipoReal tyl=TString then {exp=SCAF,ty=TInt} else error("Error de tipos", nl)
						| GeOp => if tipoReal tyl=TInt orelse tipoReal tyl=TString then {exp=SCAF,ty=TInt} else error("Error de tipos", nl)
						| _ => raise Fail "No debería pasar! (3)"
				else error("Error de tipos", nl)
			end
		| trexp(RecordExp({fields, typ}, nl)) =
			let
				(* Traducir cada expresión de fields *)
				val tfields = map (fn (sy,ex) => (sy, trexp ex)) fields

				(* Buscar el tipo *)
				val (tyr, cs) = case tabBusca(typ, tenv) of
					SOME t => (case tipoReal t of
						TRecord (cs, u) => (TRecord (cs, u), cs)
						| _ => error(typ^" no es de tipo record", nl))
					| NONE => error("Tipo inexistente ("^typ^")", nl)
				
				(* Verificar que cada campo esté en orden y tenga una expresión del tipo que corresponde *)
				fun verificar [] [] = ()
				  | verificar (c::cs) [] = error("Faltan campos", nl)
				  | verificar [] (c::cs) = error("Sobran campos", nl)
				  | verificar ((s,ref t,_)::cs) ((sy,{exp,ty})::ds) =
						if s<>sy then error("Error de campo", nl)
						else if tiposIguales ty t then verificar cs ds
							 else error("Error de tipo del campo "^s, nl)
				val _ = verificar cs tfields
			in
				{exp=SCAF, ty=tyr}
			end
		| trexp(SeqExp(s, nl)) =
			let	val lexti = map trexp s
				val exprs = map (fn{exp, ty} => exp) lexti
				val {exp, ty=tipo} = hd(rev lexti)
			in	{ exp=SCAF, ty=tipo } end
		| trexp(AssignExp({var=SimpleVar s, exp}, nl)) = (*COMPLETADO*)
			let
				val {ty=tys} = case tabBusca(s,venv) of
							SOME (Var a) => a
							| SOME _ => error("El nombre ("^s^") esta definido como una funcion",nl)
							| _ => error("Variable ("^s^") no definida",nl)
				val {exp=expexp,ty=tyexp} = trexp exp
				val _ = if tiposIguales tyexp tys then ()
						else error("Intento de asignar un valor del tipo incorrecto a la variable ("^s^")",nl)
			in {exp=SCAF, ty=TUnit} end 
		| trexp(AssignExp({var=FieldVar (v,s), exp}, nl)) = (*COMPLETADO*)
			let
				val {exp=expvar,ty=tyvar} = trvar (FieldVar (v,s),nl)
				val {exp=expexp,ty=tyexp} = trexp exp
				val _ = if tiposIguales tyvar tyexp then ()
						else error("Intentando asignar al record un tipo distinto",nl)
			in {exp=SCAF, ty=TUnit} end
		| trexp(AssignExp({var=SubscriptVar (v,e), exp}, nl)) = (*COMPLETADO*)
			let
				val {exp=expvar,ty=tyvar} = trvar (SubscriptVar (v,e),nl)
				val {exp=expexp,ty=tyexp} = trexp exp
				val _ = if tiposIguales tyvar tyexp then ()
						else error("Intentando asignar al arreglo un tipo distinto",nl)
			in {exp=SCAF, ty=TUnit} end
		| trexp(IfExp({test, then', else'=SOME else'}, nl)) =
			let val {exp=testexp, ty=tytest} = trexp test
			    val {exp=thenexp, ty=tythen} = trexp then'
			    val {exp=elseexp, ty=tyelse} = trexp else'
			in
				if tipoReal tytest=TInt andalso tiposIguales tythen tyelse then {exp=SCAF, ty=tythen}
				else error("Error de tipos en if" ,nl)
			end
		| trexp(IfExp({test, then', else'=NONE}, nl)) =
			let val {exp=exptest,ty=tytest} = trexp test
			    val {exp=expthen,ty=tythen} = trexp then'
			in
				if tipoReal tytest=TInt andalso tythen=TUnit then {exp=SCAF, ty=TUnit}
				else error("Error de tipos en if", nl)
			end
		| trexp(WhileExp({test, body}, nl)) =
			let
				val ttest = trexp test
				val tbody = trexp body
			in
				if tipoReal (#ty ttest) = TInt andalso #ty tbody = TUnit then {exp=SCAF, ty=TUnit}
				else if tipoReal (#ty ttest) <> TInt then error("Error de tipo en la condición", nl)
				else error("El cuerpo de un while no puede devolver un valor", nl)
			end
		| trexp(ForExp({var, escape, lo, hi, body}, nl)) =(*COMPLETADO*)
			let
				val {exp = explo, ty = tlo} = trexp lo
				val {exp = exphi, ty = thi} = trexp hi
				val _ = if tlo = TInt andalso thi=TInt then ()
						else error("Los indices del for deben ser enteros",nl)
				val venv' = tabRInserta(var,VIntro,venv)
				val {exp = expbody, ty = tbody} = transExp (venv',tenv) body
				val _ = if tbody = TUnit then ()
						else error("El tipo del cuerpo del for debe ser TUnit",nl)
			in {exp=SCAF, ty=TUnit} end
		| trexp(LetExp({decs, body}, _)) =
			let
				val (venv', tenv', _) = List.foldl (fn (d, (v, t, _)) => trdec(v, t) d) (venv, tenv, []) decs
				val {exp=expbody,ty=tybody}=transExp (venv', tenv') body
			in
				{exp=SCAF, ty=tybody}
			end
		| trexp(BreakExp nl) = {exp=SCAF, ty=TUnit} (*COMPLETADO*)
		| trexp(ArrayExp({typ, size, init}, nl)) = (*COMPLETADO*)
			let val t = case tabBusca(typ,tenv) of
							SOME a => a
							| NONE => error("El tipo ("^typ^") no esta definido",nl)
				val t' = case t of
					TArray (ref u,_) => u
					| _ => error("El tipo ("^typ^") no es un array",nl)
				val {exp=expsize, ty=tysize} = trexp size
				val _ = if tysize = TInt then ()
						else error("El tamaño del array debe ser un entero",nl)
				val {exp=expinit, ty=tyinit} = trexp init
				val _ = if tiposIguales tyinit t' then ()
						else error("El tipo del valor inicial no coincide con el tipo del array",nl)
			in {exp=SCAF, ty=t} end
		and trvar(SimpleVar s, nl) =(*COMPLETADO*)
			let
				val tvar = 
					case tabBusca(s,venv) of
						SOME (Var{ty}) => ty
						| SOME VIntro => TInt
						| _ => error("Variable ("^s^") no definida",nl)
			in {exp=SCAF, ty=tvar} end
		| trvar(FieldVar(v, s), nl) = (*COMPLETADO*)
			let
				val {exp=expv,ty=tyv} = trvar(v,nl)
				val l = case tyv of
							TRecord (ls,_) => ls
							| _ => error("El tipo de la variable no es un record",nl)
				val tiporesult = case List.find (fn((name,_,_)) => name = s) l of
									SOME (_,ref tipo,_) => tipo
									| NONE => error("El record no posee un campo llamado ("^s^")",nl)
			in {exp=SCAF, ty=tiporesult} end
		| trvar(SubscriptVar(v, e), nl) = (*COMPLETADO*)
			let
				val {exp=expv,ty=tyv} = trvar (v,nl)
				val t = case tyv of
						TArray (ref t',_) => t'
						| _ => error("La variable no es de tipo TArray",nl)
				val {exp=expe,ty=tye} = trexp e
				val _ = if tye = TInt then ()
						else error("El indice del array debe ser un entero",nl)
			in {exp=SCAF, ty=t} end
		and trdec (venv, tenv) (VarDec ({name,escape,typ=NONE,init},pos)) = (*COMPLETADO*)
			let 
				val {exp=expinit,ty=tyinit} = transExp (venv,tenv) init
				val _ = case tyinit of
							TNil => error("Se debe explicitar el tipo cuando se declara la variable ("^name^") y se le quiere asignar el valor nil",pos)
							| TUnit => error("No se puede declarar la variable ("^name^") y asignarle algo del tipo Unit",pos)
							| _ => ()
				val venv' = tabInserta(name,Var {ty=tyinit},venv)(*TabRInserta?*)
			in (venv', tenv, []) end (*Lista vacia para la inicializacion de variables(mas adelante)*)
		| trdec (venv,tenv) (VarDec ({name,escape,typ=SOME s,init},pos)) = (*COMPLETADO*)
			let
				val {exp=expinit,ty=tyinit} = transExp (venv,tenv) init
				val _ = if tiposIguales tyinit TUnit then error("No se puede declarar la variable ("^name^") y asignarle algo del tipo TUnit",pos)
						else ()
				val t = case tabBusca(s,tenv) of
							SOME s' => if tiposIguales s' tyinit then s'
										else error("El tipo de la declaracion de la variable ("^name^") no coincide con el tipo del valor inicial asignado",pos)
							| NONE => error("El tipo ("^s^") de la variable ("^name^") no esta definido",pos)
				val venv' = tabInserta(name,Var {ty=t},venv)(*TabRInserta?*)
			in (venv', tenv, []) end
		| trdec (venv,tenv) (FunctionDec fs) =(*COMPLETADO, ver mainLevel*)
			let
				(*Chequear que no se repitan los nombres de las funciones en el mismo batch*)
				val _ = List.foldl (fn (({name,...},pos),lb) =>
										if List.exists (fn x => x=name) lb then error("La funcion ("^name^") ya esta definida en el mismo batch",pos) 
										else name::lb) [] fs
				(*Chequear que no se repitan los nombres de los argumentos en una misma funcion*)
				val _ = List.map (fn ({name=fname,params,...},pos) =>
										List.foldl (fn (({name=pname,...}),lb) => 
														if List.exists (fn x => x=pname) lb then error("El parametro ("^pname^") esta repetido en la definicion de ("^fname^")",pos) 
														else pname::lb) [] params) fs
				(*Chequear los tipos existan y sean iguales entre result y body, luego creo la Fundec*)
				val tf = List.map (fn ({name=fname,params,result,body},pos) =>
									let	
										fun checkpar ps = List.map (fn ({name=pname,typ,...}) =>
															case typ of
																NameTy s => (case tabBusca(s,tenv) of
																				SOME t' => (pname,t')
																				| _    =>  error("El tipo del parametro ("^pname^") de la funcion ("^fname^") no esta definido",pos))
																| _ => error("Error interno relacionado al parser",pos)) ps
										val ps = checkpar params										
										val vformals = List.map (fn (name,tipo) => tipo) ps
										fun fresult res bod =
											let 
												val tresult = case res of
																SOME s => (case tabBusca(s,tenv) of
																			SOME t' => t'
																			| NONE  => error("El tipo ("^s^") no esta definido",pos))
																| NONE => TUnit
												(*val ps' = List.map (fn (name,tipo) => (name,Var {ty=tipo})) ps
												val venv' = tabInserList(venv,ps')
												val {exp,ty=tbody} = transExp (venv',tenv) bod
												val _ = if tiposIguales tresult tbody then () else error("La funcion ("^fname^") no posee el mismo tipo que su cuerpo",pos)*)
											in tresult end
										val vresult = fresult result body
									in (fname,{level=mainLevel,label=fname,formals=vformals,result=vresult,extern=false},ps,body,pos) end) fs
				val lf = List.map (fn (name,fnc,ps,body,pos) => (name,Func fnc)) tf
				val venv' = tabInserList(venv,lf)
				val _ = List.map (fn (name,{result,...},ps,body,pos) => 
									let
										val ps' = List.map (fn (name,tipo) => (name,Var {ty=tipo})) ps
										val venv_intern = tabInserList(venv',ps')
										val {exp,ty=tbody} = transExp (venv_intern,tenv) body
										val _ = if tiposIguales result tbody then () else error("La funcion ("^name^") no posee el mismo tipo que su cuerpo",pos)
									in () end) tf
			in (venv', tenv, []) end
		| trdec (venv,tenv) (TypeDec ts) = (*COMPLETADO*)
			let
				(*TypeDec of ({name: symbol, ty: ty} * pos) list*)
				(*ts => [({name,ty},pos)] ty=> NameTy string|RecordTy [field]|ArrayTy string *)
				fun buscarep [] = (false,"",0) (*Toma un nombre y se fija si aparece en el resto de la lista*)
					| buscarep (({name=s,ty},pos)::t) = if List.exists (fn({name=x,...},p) => x=s) t then (true,s,pos) else buscarep t
				val reps = buscarep ts (*Si los nombre estan repetido, tiro el error correspondiente*)
				val _ = if #1 reps then error("El nombre de tipo ("^(#2 reps)^") esta repetido",#3 reps) else ()
				val ts' = List.map (#1) ts (*Me quedo con todos los tipos, descartando sus posiciones*)
				val tenv' = (tigertopsort.fijaTipos ts' tenv)(*Llama a fijarTipos, manejando las exepciones??????*)
			in (venv, tenv, []) end 
	in trexp end
fun transProg ex =
	let	val main =
				LetExp({decs=[FunctionDec[({name="_tigermain", params=[],
								result=SOME ("int"), body=ex}, 0)]],
						body=UnitExp 0}, 0)
		val _ = transExp(tab_vars, tab_tipos) ex (*main*)
	in	print "bien!\n" end
end

