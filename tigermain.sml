open tigerlex
open tigergrm
open tigerescap
open tigerseman
open BasicIO Nonstdio

fun lexstream(is: instream) =
	Lexing.createLexer(fn b => fn n => buff_input is b 0 n);
fun errParsing(lbuf) = (print("Error en parsing!("
	^(makestring(!num_linea))^
	")["^(Lexing.getLexeme lbuf)^"]\n"); raise Fail "fin!")
fun main(args) =
	let	fun arg(l, s) =
			(List.exists (fn x => x=s) l, List.filter (fn x => x<>s) l)
		val (arbol, l1)		= arg(args, "-arbol")
		val (escapes, l2)	= arg(l1, "-escapes") 
		val (ir, l3)		= arg(l2, "-ir") 
		val (canon, l4)		= arg(l3, "-canon") 
		val (code, l5)		= arg(l4, "-code") 
		val (flow, l6)		= arg(l5, "-flow") 
		val (inter, l7)		= arg(l6, "-inter") 
		val entrada =
			case l7 of
			[n] => ((open_in n)
					handle _ => raise Fail (n^" no existe!"))
			| [] => std_in
			| _ => raise Fail "opcio'n dsconocida!"
		val lexbuf = lexstream entrada
		val expr = prog Tok lexbuf handle _ => errParsing lexbuf
		val _ = findEscape(expr)
		val _ = transProg(expr)		
		val _ = if arbol then tigerpp.exprAst expr else ()
		val lfrag = tigertrans.getResult()
		(*No necesario
		fun imprimir (tigerframe.PROC {body,frame}) = print ("PROC: "^(tigerframe.name frame)^"\n")
		    |imprimir (tigerframe.STRING (l,s)) = print ("STRING: "^l^" "^s^"\n")
		val _ = List.map imprimir lfrag*)
		fun split (l::ls) t s =
			(case l of
			 tigerframe.PROC {body,frame} => split ls (t@[(tigercanon.linearize body,frame)]) s
			 | tigerframe.STRING x => split ls t (s@[x]))
		   | split [] t s = (t,s)
		val (proclist,stringlist) = split lfrag [] []
		val _ = List.map (fn (l,s) => print ("STRING: "^l^" \""^s^"\"\n")) stringlist
		val _ = List.map (fn (b,f) => let val _ =print ("PROC: "^(tigerframe.name f)^"\n")
																			val _ = List.map (fn body => print (tigerit.tree body^"\n") ) b
																	in 0 end) proclist
		val _ = tigerinterp.inter true proclist stringlist
		(****)
	in
		print "yes!!\n"
	end	handle Fail s => print("Fail: "^s^"\n")

val _ = main(CommandLine.arguments())


