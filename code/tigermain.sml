open tigerlex
open tigergrm
open tigerescap
open tigerseman
open tigerframe
open tigercodegen
open tigerassem
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
		val (ir, l3)		  = arg(l2, "-ir")
		val (canon, l4)		= arg(l3, "-canon")
		val (code, l5)		= arg(l4, "-code")
		val (flow, l6)		= arg(l5, "-flow")
		val (inter, l7)		= arg(l6, "-inter")
		val (assem, l8)		= arg(l7, "-assem")
		val entrada =
			case l8 of
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
		fun funcanon(x) = if canon then tigercanon.traceSchedule(tigercanon.basicBlocks(tigercanon.linearize x))
								   else tigercanon.linearize x
		fun split (l::ls) t s =
			(case l of
			   tigerframe.PROC {body,frame} => split ls (t@[(funcanon body,frame)]) s
			 | tigerframe.STRING x => split ls t (s@[x]))
		   | split [] t s = (t,s)
		val (proclist,stringlist) = split lfrag [] []
		val _ = if code then
							let
								val _ = List.map (fn (l,s) => print ("STRING: "^l^" \""^s^"\"\n")) stringlist
								val _ = List.map (fn (b,f) => let val _ = print ("PROC: "^(tigerframe.name f)^" args: ")
																  val _ = map (fn (InReg label) => print("TEMP "^label^"; ")
																                | (InFrame k)=> print("MEM "^Int.toString(k)^"; ")) (tigerframe.formals f)
																  val _ = print("\n")
																  val _ = List.map (fn body => (print(tigerit.tree body);(codegen f body);print("\n"))) b
															  in 0 end) proclist
							in () end
						else ()
        val _ = if assem then let fun flat xs = List.foldr (fn (x, acc) => x @ acc) [] xs
                                  fun escape s = List.foldr (fn (x, acc) => case x of
                                                                             #"\n" => "\\n"^acc
                                                                            |  _  => String.str(x)^acc) "" (String.explode s)
                                  val _ = print("##START##\n")
                                  val _ = print("\n.data\n")
                                  val _ = List.map (fn (l,s) => print(l^":\t.quad "^Int.toString(String.size s)^"\n\t.string \""^(escape s)^"\"\n")) stringlist
                                  val _ = print("\n.text\n")
					              val proc_l = List.map (fn (bl,f) => let val il = List.map (fn b => codegen f b) bl in (procEntryExit3(f,flat il)) end) proclist
                                  val _ = List.map (fn proc => let val _ = print(#prolog proc)
                                                                   val instrlist = (List.map showCode (#body proc)) @ ["\n\n"]
                                                                   val instructions = List.foldr (fn (x, acc) => ("\t"^x^"\n")^acc) "" instrlist
                                                                   val _ = print(instructions)
                                                                   val _ = print(#epilog proc)
                                                                in () end) proc_l
							  in print("##FINISH##\n") end
				else ()
		val _ = if inter then tigerinterp.inter flow proclist stringlist
						else ()
	in
		print "yes!!\n"
	end	handle Fail s => print("Fail: "^s^"\n")

val _ = main(CommandLine.arguments())
